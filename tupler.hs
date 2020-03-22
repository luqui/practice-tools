{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections, LambdaCase #-}

import qualified System.MIDI as MIDI
import Control.Monad (filterM, guard, forM_, ap, join, (<=<), when)
import Data.Ratio
import Data.List (inits, tails, genericLength, genericReplicate, transpose)
import qualified Control.Monad.Random as Rand
import qualified Data.Time.Clock.POSIX as Clock
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.Tuple (swap)
import Data.IORef
import Data.Monoid (Any(..))
import qualified System.IO as IO
import System.Environment (getArgs)
import Data.Foldable (traverse_)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

newtype Cloud a = Cloud { getCloud :: [(a, Rational)] }
    deriving (Functor)

instance Applicative Cloud where
    pure x = Cloud [(x, 1)]
    (<*>) = ap

instance Monad Cloud where
    Cloud ps >>= f = Cloud $ do
        (x,p) <- ps
        (y,q) <- getCloud (f x)
        pure (y, p*q)

pchoice :: [(Rational, a)] -> Cloud a
pchoice = Cloud . map swap

puniform :: [a] -> Cloud a
puniform xs = Cloud (map (, 1 % genericLength xs) xs)

pfilter :: (a -> Bool) -> Cloud a -> Cloud a
pfilter p = Cloud . filter (p . fst) . getCloud
    


data Beat a = Beat { bSubdiv :: Rational, bHits :: [a] }
    deriving (Eq, Ord, Show, Functor)

mkBeat :: (Eq a) => Rational -> [a] -> Beat a
mkBeat s h = Beat s (cycleGenerator h)

rGcd :: Rational -> Rational -> Rational
rGcd r r' = gcd (numerator r) (numerator r') % lcm (denominator r) (denominator r')

superpose :: (Eq a, Eq b) => Beat a -> Beat b -> Beat (Maybe a, Maybe b)
superpose (Beat subdiv hits) (Beat subdiv' hits') = 
    Beat grid $ zip (concat [ Just x : replicate (fromIntegral spacing-1)  Nothing | x <- concat (genericReplicate (umbrella `div` total) hits)  ])
                    (concat [ Just y : replicate (fromIntegral spacing'-1) Nothing | y <- concat (genericReplicate (umbrella `div` total') hits') ])
    where
    grid = rGcd subdiv subdiv'
    spacing = toZ (subdiv/grid)
    spacing' = toZ (subdiv'/grid)
    total = spacing * genericLength hits
    total' = spacing' * genericLength hits'
    umbrella = lcm total total'

    toZ r | denominator r == 1 = numerator r
          | otherwise = error (show r ++ " is not an integer")

cycleGenerator :: (Eq a) => [a] -> [a]
cycleGenerator xs = head [ pre | pre <- tail (inits xs), length xs `mod` length pre == 0, xs == zipWith const (cycle pre) xs ]

showBeat :: (Rational, Beat [Char]) -> String
showBeat (met,beat) = unlines (tempo : meter : transpose (bHits beat))
    where
    tempo = show (floor (60 / met) :: Integer) ++ " bpm"
    meter = show (numerator subdiv) ++ "/" ++ show (denominator subdiv)
    subdiv = bSubdiv beat / met

data Exercise = Exercise
    { exMetronome :: Rational
    , exBeat :: Beat Bool 
    }
    deriving (Eq,Ord)

showExercise :: Exercise -> String
showExercise (Exercise met beat) = showBeat . (met,) . fmap (\(h,m) -> [hitCh h, metCh m]) $ guide
    where
    guide = superpose beat (Beat met [()])

    metCh (Just ()) = '|'
    metCh Nothing = '.'

    hitCh (Just True) = '*'
    hitCh (Just False) = '.'
    hitCh Nothing = ' '

motions :: Rational -> Map.Map Exercise Integer -> Exercise -> Cloud Exercise
motions targetDiff seen (Exercise met (Beat subdiv hits)) = weightify <=< pfilter valid . join . puniform $
    [ dup, slice, addOrRemove, halfTime, doubleTime, remeter, prune, rot, split, trim ]
    where
    dup = Exercise met . Beat subdiv <$> puniform [hits, hits ++ hits, hits ++ hits ++ hits ]  -- breaking invariant briefly...
    slice = fmap (Exercise met) . puniform $ do
        offs <- [-2,-1,1,2]
        let len = offs + length hits
        guard $ len > 1
        pure $ mkBeat subdiv (take len (cycle hits))
    addOrRemove = fmap (Exercise met) . puniform $ do
        (pre, x:post) <- zip (inits hits) (tails hits)
        let hits' = pre ++ not x : post
        pure $ mkBeat subdiv hits'
    halfTime = fmap (Exercise met) . pure $ mkBeat (subdiv/2) hits
    doubleTime = fmap (Exercise met) . pure $ mkBeat (subdiv*2) hits
    remeter = puniform $ do
        ratio <- [1/3, 1/2, 2/3, 3/4, 4/3, 3/2, 2, 3]
        pure $ Exercise (met * ratio) (mkBeat subdiv hits)
    rot = fmap (Exercise met) . puniform $ do
        r <- [1..length hits-1]
        pure $ mkBeat subdiv (take (length hits) . drop r $ cycle hits)
    split = fmap (Exercise met) . puniform $ do
        d <- [2,3,5]
        pure $ mkBeat (subdiv/fromIntegral d) (concat [ h : replicate (d-1) False | h <- hits ])
    prune = fmap (Exercise met) . puniform $ do
        d <- [2,3]
        o <- [0..d-1]
        let guide = replicate o False ++ cycle (True : replicate (d-1) False)
        guard . and $ zipWith (\a b -> a || not b) guide hits
        pure $ mkBeat (fromIntegral d * subdiv) (map snd . filter fst $ zip guide hits)
    trim = fmap (Exercise met) . puniform $ do
        (pre, _:post) <- tail . init $ zip (inits hits) (tails hits)
        pure $ mkBeat subdiv (pre ++ post)

    weightify ex = Cloud [(ex, 1 / (1 + (exerciseWeight ex - targetDiff)^(2::Int))) ]

    valid (Exercise m (Beat s h)) = and
        [ s' <= 1
        , s < 1
        , or h
        , length h' <= 80
        , 1/2 <= m && m <= 2
        , genericLength h' * s' < 5  -- no more than 5 seconds per pattern
        , approxExercise (Exercise m (Beat s h)) `Map.notMember` seen
        ]
        where
        Beat s' h' = superpose (Beat s h) (Beat m [()])

exerciseWeight :: Exercise -> Rational
exerciseWeight (Exercise met (Beat s h)) = sum
    [ genericLength h ^ (2 :: Int)
    , genericLength h'
    , fromIntegral (denominator (s/met) ^ (2::Int))
    , fromIntegral (denominator (s'/met) ^ (2::Int))
    , 2 * ((60 / met - 100) / 30) ^ (2::Int)  -- 100 bpm is comfortable
    , 2 * ((60 / s - 200) / 60) ^ (2::Int)  -- 200 notes per minute is comfortable
    ]
    where
    Beat s' h' = superpose (Beat s h) (Beat met [()])


divisors :: (Integral a) => a -> [a]
divisors n = [ m | m <- [1..n], n `mod` m == 0 ]

playBeat :: MIDI.Connection -> Scorer -> Beat (Any, [Int]) -> Clock.POSIXTime -> IO Clock.POSIXTime
playBeat dest scorer beat t0 = do
    let subdiv = realToFrac (bSubdiv beat)
    let finalTime = t0 + subdiv * genericLength (bHits beat)

    _ <- forkIO $ do
        waitUntil t0
        forM_ (zip [t0, t0 + subdiv..] (bHits beat)) $ \(t,(expect, notes)) -> do
            when (getAny expect) $ scoOnMet scorer t
            forM_ notes $ \n -> do
                MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn n 100))
            waitUntil $ t + subdiv
            forM_ notes $ \n -> MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn n 0))

    pure finalTime

playExercise :: MIDI.Connection -> Scorer -> Bool -> Exercise -> Clock.POSIXTime -> IO Clock.POSIXTime
playExercise dest scorer playGuide (Exercise met b) t0 = do
    tf <- playBeat dest scorer play t0
    let go = do
            now <- Clock.getPOSIXTime
            let waitMillis = floor (1000 * (tf - now))
            ready <- IO.hWaitForInput IO.stdin (max 0 waitMillis)
            when ready $ do
                _ <- getChar
                scoOnHit scorer =<< Clock.getPOSIXTime
                go
    go
    waitUntil tf
    pure tf
  where
    guide = superpose b (Beat met [()])
    play | playGuide = fmap (\(h,m) -> metNote m <> hitNote h) guide
         | otherwise = fmap (\(h,m) -> metNote m <> silentHitNote h) guide

    metNote (Just ()) = (Any False, [56])
    metNote Nothing = mempty

    hitNote (Just True) = (Any True, [42])
    hitNote _ = mempty

    silentHitNote (Just True) = (Any True, [])
    silentHitNote _ = mempty
    

waitUntil :: Clock.POSIXTime -> IO ()
waitUntil t = do
    now <- Clock.getPOSIXTime
    threadDelay (floor (1000000 * (t - now)))

openDest :: String -> IO MIDI.Connection
openDest name = do
    cand <- fmap head . filterM (fmap (== name) . MIDI.getName) =<< MIDI.enumerateDestinations 
    print cand
    MIDI.openDestination cand

data Scorer = Scorer
    { scoOnMet :: Clock.POSIXTime -> IO ()
    , scoOnHit :: Clock.POSIXTime -> IO ()
    , scoGetScore :: IO Double
    , scoReset :: IO ()
    , scoPerfect :: IO Bool
    }

makeScorer :: MIDI.Connection -> IO Scorer
makeScorer dest = do
    time0 <- Clock.getPOSIXTime
    stateRef <- newIORef (time0, [])
    scoreRef <- newIORef (0 :: Int, 100 :: Double, True)

    changeScore <- pure $ \r -> do
        (combo, score, perf) <- readIORef scoreRef
        putStr $ "\o33[1G\o33[K"
        if | r > 0.9 -> do
            putStr $ "\o33[1;32m+" ++ show (fromIntegral (combo+1) * r) ++ " (x " ++ show (combo+1) ++ ")\o33[0m"
            writeIORef scoreRef (combo + 1, score + fromIntegral combo * r, perf)
           | r >= 0 -> do
            putStr $ "\o33[1;33m+" ++ show (fromIntegral combo * r) ++ " (x " ++ show combo ++ ")\o33[0m"
            writeIORef scoreRef (combo, score + fromIntegral combo * r, perf)
           | otherwise -> do
            putStr $ "\o33[1;31m" ++ show r ++ "\o33[0m"
            writeIORef scoreRef (0, score + r, False)

    measure <- pure $ \t t' -> 2 * exp ( -(15*realToFrac (t - t'))^(2::Int) ) - 1

    applyMeasure <- pure $ \r ->
        if | r < 0 -> changeScore (10 * r)
           | otherwise -> changeScore r

    onMet' <- pure $ \t -> do
        (_, hits) <- readIORef stateRef
        traverse_ (applyMeasure . measure t) hits
        writeIORef stateRef (t, [])
    
    onHit' <- pure $ \h -> do
        MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn 37 100))
        MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn 37 0))
        (met, hits) <- readIORef stateRef
        if | measure met h > 0 -> applyMeasure (measure met h)
           | otherwise -> writeIORef stateRef (met, h:hits)

    pure $ Scorer onMet' onHit'
                ((\(_,score,_) -> score) <$> readIORef scoreRef)
                (modifyIORef scoreRef (\(a,b,_) -> (a,b,True)))
                ((\(_,_,perf) -> perf)<$> readIORef scoreRef)

approxExercise :: Exercise -> Exercise
approxExercise (Exercise met b) = Exercise (fromInteger (floor ((60 / met) / 15))) b

normalMode :: MIDI.Connection -> Scorer -> Integer -> Exercise -> Exercise -> Clock.POSIXTime -> IO (Bool, Clock.POSIXTime)
normalMode dest scorer level ex nextEx t0 = do
    scoReset scorer
    putStrLn "\o33[2J\o33[1;1f"

    score <- scoGetScore scorer
    putStrLn $ "Level " ++ show level
    putStrLn $ "\o33[1;32mScore: " ++ show score ++ "\o33[0m"
    putStrLn $ "Difficulty: " ++ show (realToFrac (exerciseWeight ex) :: Double)
    putStrLn $ showExercise ex
    when (exMetronome nextEx /= exMetronome ex) $ putStrLn "\o33[1;31mNEW TEMPO\o33[0m"
    putStrLn $ showExercise nextEx

    t4 <- do
        t1 <- playExercise dest scorer False ex t0
        t2 <- playExercise dest scorer False ex t1
        t3 <- playExercise dest scorer False ex t2
        t4 <- playExercise dest scorer False ex t3
        pure t4

    perf <- scoPerfect scorer
    pure (perf, t4)

practiceMode :: MIDI.Connection -> Scorer -> Integer -> Exercise -> Clock.POSIXTime -> IO Clock.POSIXTime
practiceMode conn scorer level ex _ = do
    let countIn ex' t0 = do
            t1 <- playExercise conn scorer False (Exercise (exMetronome ex') (const False <$> exBeat ex')) t0
            t2 <- playExercise conn scorer False (Exercise (exMetronome ex') (const False <$> exBeat ex')) t1
            pure t2
            
    let tryEx ex' nextEx' t0 = do
            t1 <- countIn ex' t0
            (win,t2) <- normalMode conn scorer level ex' nextEx' t1
            if win then pure t2 else practiceMode conn scorer level ex' t2

    let showMenu = do
            putStrLn "\o33[2J\o33[1;1f"
            putStrLn "Practice mode"
            putStrLn $ showExercise ex

            putStrLn "(r) Retry"
            putStrLn "(s) Slower Tempo"
            putStrLn "(<number>) Subdivide Metronome"

            let cont t = do
                    (win,t1) <- normalMode conn scorer level ex ex t 
                    if win then pure t1 else showMenu
            opt <- getChar
            t0 <- Clock.getPOSIXTime
            case opt of
                'r' -> countIn ex t0 >>= cont
                's' -> tryEx (scaleTime (4/3) ex) ex t0 >>= cont
                n | Char.isDigit n, read [n] > (0 :: Int)
                    -> tryEx (scaleMetronome (1 % read [n]) ex) ex t0 >>= cont
                _ -> showMenu
    showMenu

scaleTime :: Rational -> Exercise -> Exercise
scaleTime s (Exercise m (Beat subdiv hits)) = Exercise (s*m) (Beat (s*subdiv) hits)

scaleMetronome :: Rational -> Exercise -> Exercise
scaleMetronome s (Exercise m b) = Exercise (s*m) b

main :: IO ()
main = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho IO.stdin False

    dest <- openDest "IAC Bus 1"
    now <- Clock.getPOSIXTime
    level0 <- getArgs >>= pure . \case
                [] -> 0
                [n] -> read n
                _ -> error "Unknown argument"
    scorer <- makeScorer dest
    tempo0 <- Rand.evalRandIO $ Rand.uniform [60/t | t <- [60..120]]
    time0 <- playBeat dest scorer (Beat tempo0 (replicate 4 (Any False, [56]))) now
    evalStateT (go dest scorer Map.empty level0 (Exercise tempo0 (Beat tempo0 [True]))) time0
    where
    go :: MIDI.Connection -> Scorer -> Map.Map Exercise Integer -> Integer -> Exercise -> StateT Clock.POSIXTime IO ()
    go dest scorer seen level ex = do
        let targetDiff = 5 * fromIntegral level :: Double
        let options = getCloud (motions (realToFrac targetDiff) seen ex)
        nextEx <- lift $ if not (null options) then Rand.evalRandIO (Rand.weighted options) else pure ex

        let thisLevel = StateT $ normalMode dest scorer level ex nextEx
        let nextLevel = go dest scorer (Map.insertWith (+) (approxExercise nextEx) 1 seen) (level+1) nextEx
        let chain 0 = do
                () <- StateT $ (fmap.fmap) ((),) $ practiceMode dest scorer level ex
                nextLevel
            chain n = do
                win <- thisLevel
                if win then nextLevel else chain (n-1)
        chain (3 :: Int)


