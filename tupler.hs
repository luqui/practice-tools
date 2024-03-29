{-# OPTIONS -Wall -Wno-type-defaults #-}
{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections, LambdaCase #-}

import qualified System.MIDI as MIDI
import Control.Monad (filterM, guard, forM_, ap, join, (<=<), when)
import Control.Arrow (second)
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

alterFirst :: (a -> a) -> Beat a -> Beat a
alterFirst f (Beat subdiv (h:hits)) = Beat subdiv (f h : hits)
alterFirst _ (Beat _ []) = error "Beat should have at least one hit?"

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
        -- At least half as many hits as metronome hits because this thing
        -- has a penchant for picking really boring rhythms
        , 2 * length [ () | (Just True,_) <- h' ] >= length [ () | (_, Just _) <- h' ] 
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

playBeat :: MIDIConns -> Scorer -> Beat (Any, [Int]) -> Clock.POSIXTime -> IO Clock.POSIXTime
playBeat conns scorer beat t0 = do
    let subdiv = realToFrac (bSubdiv beat)
    let finalTime = t0 + subdiv * genericLength (bHits beat)

    _ <- forkIO $ do
        waitUntil t0
        forM_ (zip [t0, t0 + subdiv..] (bHits beat)) $ \(t,(expect, notes)) -> do
            when (getAny expect) $ scoOnMet scorer t
            forM_ notes $ \n -> do
                MIDI.send (midiOut conns) (MIDI.MidiMessage 1 (MIDI.NoteOn n 64))
                waitUntil $ t + min (1/16) subdiv
                MIDI.send (midiOut conns) (MIDI.MidiMessage 1 (MIDI.NoteOn n 0))
            waitUntil $ t + subdiv
            forM_ notes $ \n -> MIDI.send (midiOut conns) (MIDI.MidiMessage 1 (MIDI.NoteOn n 0))

    pure finalTime

playExercise :: MIDIConns -> Scorer -> Bool -> Exercise -> Clock.POSIXTime -> IO Clock.POSIXTime
playExercise conns scorer playGuide (Exercise met b) t0 = do
    tf <- playBeat conns scorer play t0
    let go = do
            now <- Clock.getPOSIXTime
            when (now < tf) $ do
                nextEvent <- MIDI.getNextEvent (midiIn conns)
                case nextEvent of
                    Nothing -> threadDelay 1000 
                    Just (MIDI.MidiEvent _ (MIDI.MidiMessage _ (MIDI.NoteOn _ v))) | v /= 0 ->
                        scoOnHit scorer =<< Clock.getPOSIXTime
                    _ -> pure ()
                go

            {-
            ready <- IO.hWaitForInput IO.stdin (max 0 waitMillis)
            when ready $ do
                _ <- getChar
                scoOnHit scorer =<< Clock.getPOSIXTime
                go
            -}
    go
    waitUntil tf
    pure tf
  where
    guide = superpose b (Beat met [()])
    play | playGuide = alterFirst (second (map (+12))) $ fmap (\(h,m) -> metNote m <> hitNote h) guide
         | otherwise = alterFirst (second (map (+12))) $ fmap (\(h,m) -> metNote m <> silentHitNote h) guide

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

openSource :: String -> IO MIDI.Connection
openSource name = do
    cand <- fmap head . filterM (fmap (== name) . MIDI.getName) =<< MIDI.enumerateSources
    print cand
    conn <- MIDI.openSource cand Nothing
    MIDI.start conn
    pure conn

data Scorer = Scorer
    { scoOnMet :: Clock.POSIXTime -> IO ()
    , scoOnHit :: Clock.POSIXTime -> IO ()
    , scoInitState :: IO ()
    , scoGetScore :: IO Double
    , scoReset :: IO ()
    , scoPerfect :: IO Bool
    }

data MIDIConns = MIDIConns
    { midiIn :: MIDI.Connection
    , midiOut :: MIDI.Connection
    }

makeScorer :: MIDIConns -> IO Scorer
makeScorer conns = do
    stateRef <- newIORef (Nothing, Nothing)
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

    initState' <- pure $ writeIORef stateRef (Nothing, Nothing)

    applyMeasure <- pure $ \r ->
        if | r < 0 -> changeScore (10 * r)
           | otherwise -> changeScore r

    onMet' <- pure $ \t -> do
        (mmet, hits) <- readIORef stateRef
        case (mmet,hits) of
            (Nothing, Nothing) -> writeIORef stateRef (Just t, Nothing)
            (Just t', Nothing)  -> changeScore (-10) >> writeIORef stateRef (Just t', Nothing)
            (Nothing, Just h)
                | measure t h > 0 -> applyMeasure (measure t h) >> writeIORef stateRef (Nothing, Nothing)
                | otherwise       -> changeScore (-10)          >> writeIORef stateRef (Just t, Nothing)
            _ -> error "impossible"
    
    onHit' <- pure $ \h -> do
        --MIDI.send (midiOut conns) (MIDI.MidiMessage 1 (MIDI.NoteOn 37 100))
        --MIDI.send (midiOut conns) (MIDI.MidiMessage 1 (MIDI.NoteOn 37 0))
        (mmet, hits) <- readIORef stateRef
        case (mmet, hits) of
            (Nothing, Nothing) -> writeIORef stateRef (Nothing, Just h)
            (Nothing, Just _) -> changeScore (-10) >> writeIORef stateRef (Nothing, Just h)
            (Just t, Nothing)
                | measure t h > 0 -> applyMeasure (measure t h) >> writeIORef stateRef (Nothing, Nothing)
                | otherwise -> changeScore (-10) >> writeIORef stateRef (Nothing, Just h)
            _ -> error "impossible"

    pure $ Scorer onMet' onHit' initState'
                ((\(_,score,_) -> score) <$> readIORef scoreRef)
                (modifyIORef scoreRef (\(a,b,_) -> (a,b,True)))
                ((\(_,_,perf) -> perf)<$> readIORef scoreRef)

approxExercise :: Exercise -> Exercise
approxExercise (Exercise met b) = Exercise (fromInteger (floor ((60 / met) / 15))) b

normalMode :: MIDIConns -> Scorer -> Integer -> Exercise -> Exercise -> Clock.POSIXTime -> IO (Bool, Clock.POSIXTime)
normalMode conns scorer level ex nextEx t0 = do
    putStrLn "\o33[2J\o33[1;1f"

    score <- scoGetScore scorer
    putStrLn $ "Level " ++ show level
    putStrLn $ "\o33[1;32mScore: " ++ show score ++ "\o33[0m"
    putStrLn $ "Difficulty: " ++ show (realToFrac (exerciseWeight ex) :: Double)
    putStrLn $ showExercise ex
    when (exMetronome nextEx /= exMetronome ex) $ putStrLn "\o33[1;31mNEW TEMPO\o33[0m"
    putStrLn $ showExercise nextEx

    let go 4 _ tin = pure (True, tin)
        go _ 4 tin = pure (False, tin)
        go wins losses tin = do
            putStrLn $ "    " ++ show wins ++ " / " ++ show losses
            scoReset scorer
            t1 <- playExercise conns scorer False ex tin
            perf <- scoPerfect scorer
            if perf then
                go (wins+1) 0 t1
            else
                go 0 (losses+1) t1
    go 0 0 t0

practiceMode :: MIDIConns -> Scorer -> Integer -> Exercise -> Clock.POSIXTime -> IO Clock.POSIXTime
practiceMode conns scorer level ex _ = do
    let countIn ex' t0 = do
            t1 <- playExercise conns scorer False (Exercise (exMetronome ex') (const False <$> exBeat ex')) t0
            t2 <- playExercise conns scorer False (Exercise (exMetronome ex') (const False <$> exBeat ex')) t1
            pure t2
            
    let tryEx ex' nextEx' t0 = do
            scoInitState scorer
            t1 <- countIn ex' t0
            (win,t2) <- normalMode conns scorer level ex' nextEx' t1
            if win then pure t2 else practiceMode conns scorer level ex' t2

    let showMenu = do
            putStrLn "\o33[2J\o33[1;1f"
            putStrLn "Practice mode"
            putStrLn $ showExercise ex

            putStrLn "(r) Retry"
            putStrLn "(s) Slower Tempo"
            putStrLn "(<number>) Subdivide Metronome"
            putStrLn "(k) Skip"

            let cont t = do
                    (win,t1) <- normalMode conns scorer level ex ex t 
                    if win then pure t1 else showMenu
            opt <- getChar
            t0 <- Clock.getPOSIXTime
            case opt of
                'r' -> countIn ex t0 >>= cont
                's' -> tryEx (scaleTime (4/3) ex) ex t0 >>= cont
                'k' -> pure t0
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
    IO.hSetBuffering IO.stdout IO.NoBuffering
    IO.hSetEcho IO.stdin False

    putStrLn "Sources"
    putStrLn "-------"
    mapM_ (putStrLn <=< MIDI.getName) =<< MIDI.enumerateSources

    putStrLn ""
    putStrLn "Destinations"
    putStrLn "------------"
    mapM_ (putStrLn <=< MIDI.getName) =<< MIDI.enumerateDestinations

    [sourceName, destName] <- getArgs

    conns <- MIDIConns <$> openSource sourceName <*> openDest destName
    now <- Clock.getPOSIXTime
    level0 <- getArgs >>= pure . \case
                [_,_] -> 0
                [_,_,n] -> read n
                _ -> error "Unknown argument"
    scorer <- makeScorer conns
    tempo0 <- Rand.evalRandIO $ Rand.uniform [60/t | t <- [60..120]]
    time0 <- playBeat conns scorer (Beat tempo0 (replicate 4 (Any False, [56]))) now
    evalStateT (go conns scorer Map.empty level0 (Exercise tempo0 (Beat tempo0 [True]))) time0
    where
    go :: MIDIConns -> Scorer -> Map.Map Exercise Integer -> Integer -> Exercise -> StateT Clock.POSIXTime IO ()
    go conns scorer seen level ex = do
        let targetDiff = 5 * fromIntegral level :: Double
        let options = getCloud (motions (realToFrac targetDiff) seen ex)
        nextEx <- lift $ if not (null options) then Rand.evalRandIO (Rand.weighted options) else pure ex

        let thisLevel = StateT $ normalMode conns scorer level ex nextEx
        let nextLevel = go conns scorer (Map.insertWith (+) (approxExercise nextEx) 1 seen) (level+1) nextEx
        win <- thisLevel
        when (not win) . StateT . (fmap.fmap) ((),) $ practiceMode conns scorer level ex
        nextLevel
