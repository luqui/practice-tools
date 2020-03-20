{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections, LambdaCase #-}

import qualified System.MIDI as MIDI
import Control.Monad (filterM, guard, forM_, ap, join, (<=<), when, forever)
import Data.Ratio
import Data.List (inits, tails, genericLength, genericReplicate, transpose)
import qualified Control.Monad.Random as Rand
import qualified Data.Time.Clock.POSIX as Clock
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Map as Map
import Data.Tuple (swap)
import Data.IORef
import Data.Monoid (Any(..))
import qualified System.IO as IO
import System.Exit (exitSuccess)
import System.Environment (getArgs)

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

approxTempo :: Rational -> Rational
approxTempo met = fromInteger (floor ((60 / met) / 15))

motions :: Rational -> Map.Map (Rational, Beat Bool) Integer -> (Rational, Beat Bool) -> Cloud (Rational, Beat Bool)
motions targetDiff seen (met, Beat subdiv hits)  = weightify <=< pfilter valid . join . puniform $
    [ dup, slice, addOrRemove, halfTime, doubleTime, remeter, prune, rot, split, trim ]
    where
    dup = (met,) . Beat subdiv <$> puniform [hits, hits ++ hits, hits ++ hits ++ hits ]  -- breaking invariant briefly...
    slice = fmap (met,) . puniform $ do
        offs <- [-2,-1,1,2]
        let len = offs + length hits
        guard $ len > 1
        pure $ mkBeat subdiv (take len (cycle hits))
    addOrRemove = fmap (met,) . puniform $ do
        (pre, x:post) <- zip (inits hits) (tails hits)
        let hits' = pre ++ not x : post
        pure $ mkBeat subdiv hits'
    halfTime = fmap (met,) . pure $ mkBeat (subdiv/2) hits
    doubleTime = fmap (met,) . pure $ mkBeat (subdiv*2) hits
    remeter = puniform $ do
        ratio <- [1/3, 1/2, 2/3, 3/4, 4/3, 3/2, 2, 3]
        pure (met * ratio, mkBeat subdiv hits)
    rot = fmap (met,) . puniform $ do
        r <- [1..length hits-1]
        pure $ mkBeat subdiv (take (length hits) . drop r $ cycle hits)
    split = fmap (met,) . puniform $ do
        d <- [2,3,5]
        pure $ mkBeat (subdiv/fromIntegral d) (concat [ h : replicate (d-1) False | h <- hits ])
    prune = fmap (met,) . puniform $ do
        d <- [2,3]
        o <- [0..d-1]
        let guide = replicate o False ++ cycle (True : replicate (d-1) False)
        guard . and $ zipWith (\a b -> a || not b) guide hits
        pure $ mkBeat (fromIntegral d * subdiv) (map snd . filter fst $ zip guide hits)
    trim = fmap (met,) . puniform $ do
        (pre, _:post) <- tail . init $ zip (inits hits) (tails hits)
        pure $ mkBeat subdiv (pre ++ post)

    weightify (m,b) = Cloud [((m,b), 1 / (1 + (beatWeight (m,b) - targetDiff)^(2::Int))) ]

    valid (m, Beat s h) = and
        [ s' <= 1
        , s < 1
        , or h
        , length h' <= 80
        , 1/2 <= m && m <= 2
        , genericLength h' * s' < 5  -- no more than 5 seconds per pattern
        , (approxTempo m, Beat s h) `Map.notMember` seen
        -- , let w = beatWeight (m, Beat s h) in targetDiff - 20 < w && w < targetDiff + 20
        ]
        where
        Beat s' h' = superpose (Beat s h) (Beat m [()])

beatWeight :: (Rational, Beat Bool) -> Rational
beatWeight (met, Beat s h) = sum
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
    waitUntil t0
    _ <- forkIO $ do
        forM_ (zip [t0, t0 + subdiv..] (bHits beat)) $ \(t,(expect, notes)) -> do
            when (getAny expect) $ onMet scorer t
            forM_ notes $ \n -> do
                MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn n 100))
            waitUntil $ t + subdiv
            forM_ notes $ \n -> MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn n 0))

    let finalTime = t0 + subdiv * genericLength (bHits beat)
    waitUntil finalTime
    pure finalTime

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
    { onMet :: Clock.POSIXTime -> IO ()
    , onHit :: Clock.POSIXTime -> IO ()
    , getScore :: IO Double
    , getLives :: IO Int
    , reset :: IO ()
    , perfect :: IO Bool
    }

makeScorer :: MIDI.Connection -> IO Scorer
makeScorer dest = do
    stateRef <- newIORef (Nothing, Nothing)
    scoreRef <- newIORef (0 :: Int, 100 :: Double, 3 :: Int, False, True)

    changeScore <- pure $ \r -> do
        (combo, score, lives, lifeGuard, perf) <- readIORef scoreRef
        putStr $ "\o33[1G\o33[K"
        if | r > 0.9 -> do
            putStr $ "\o33[1;32m+" ++ show (fromIntegral (combo+1) * r) ++ " (x " ++ show (combo+1) ++ ")\o33[0m"
            writeIORef scoreRef (combo + 1, score + fromIntegral combo * r, lives, lifeGuard, perf)
           | r >= 0 -> do
            putStr $ "\o33[1;33m+" ++ show (fromIntegral combo * r) ++ " (x " ++ show combo ++ ")\o33[0m"
            writeIORef scoreRef (combo, score + fromIntegral combo * r, lives, lifeGuard, perf)
           | otherwise -> do
            putStr $ "\o33[1;31m" ++ show r ++ "\o33[0m"
            writeIORef scoreRef (0, score + r, if lifeGuard then lives else lives {- - 1 -}, True, False)
        writeIORef stateRef (Nothing, Nothing)

    measure <- pure $ \t t' -> do
        let r = 2 * exp ( -(15*realToFrac (t - t'))^(2::Int) ) - 1
        if | r < 0 -> changeScore (10 * r)
           | otherwise -> changeScore r

        
    onMet' <- pure $ \t -> do
        state <- readIORef stateRef
        case state of
            (Nothing, Just h) -> measure t h
            (Nothing, Nothing) -> writeIORef stateRef (Just t, Nothing)
            (Just _t', Nothing) -> changeScore (-10) >> writeIORef stateRef (Just t, Nothing)
            (Just _t', Just _h') -> error "Impossible!"
    
    onHit' <- pure $ \h -> do
        state <- readIORef stateRef
        case state of
            (Just t, Nothing) -> measure t h
            (Nothing, Nothing) -> writeIORef stateRef (Nothing, Just h)
            (Nothing, Just _h') -> changeScore (-10) >> writeIORef stateRef (Nothing, Just h)
            (Just _t', Just _h') -> error "Impossible!"

    _ <- forkIO $ do
        IO.hSetBuffering IO.stdin IO.NoBuffering
        IO.hSetEcho IO.stdin False
        forever $ do
            _ <- getChar
            MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn 37 100))
            MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn 37 0))
            onHit' =<< Clock.getPOSIXTime

    pure $ Scorer onMet' onHit'
                ((\(_,score,_,_,_) -> score) <$> readIORef scoreRef)
                ((\(_,_,lives,_,_) -> lives) <$> readIORef scoreRef) 
                (modifyIORef scoreRef (\(a,b,c,_,_) -> (a,b,c,False,True)))
                ((\(_,_,_,_,perf) -> perf)<$> readIORef scoreRef)

main :: IO ()
main = do
    dest <- openDest "IAC Bus 1"
    now <- Clock.getPOSIXTime
    level0 <- getArgs >>= pure . \case
                [] -> 0
                [n] -> read n
                _ -> error "Unknown argument"
    scorer <- makeScorer dest
    tempo0 <- Rand.evalRandIO $ Rand.uniform [60/t | t <- [60..120]]
    time0 <- playBeat dest scorer (Beat tempo0 (replicate 4 (Any False, [56]))) now
    go dest scorer Map.empty level0 time0 (tempo0, Beat tempo0 [True]) False
    where
    go :: MIDI.Connection -> Scorer -> Map.Map (Rational, Beat Bool) Integer -> Integer -> Clock.POSIXTime -> (Rational, Beat Bool) -> Bool -> IO ()
    go dest scorer seen level t0 (met, b) playGuide = do
        reset scorer
        putStrLn "\o33[2J\o33[1;1f"
        score <- getScore scorer
        lives <- getLives scorer
        let targetDiff = 5 * fromIntegral level :: Double
        putStrLn $ "Level " ++ show level ++ " (diff " ++ show (floor targetDiff :: Integer) ++ ")\n"
        putStrLn $ "\o33[1;33mLives: " ++ show lives ++ "\o33[0m\n"
        putStrLn $ "\o33[1;32mScore: " ++ show score ++ "\o33[0m"
        when (lives < 0) (putStrLn "GAME OVER" >> exitSuccess)
        let guide = superpose b (Beat met [()])
        putStrLn $ "Difficulty: " ++ show (realToFrac (beatWeight (met, b)) :: Double)
        putStrLn . showBeat . (met,) . fmap (\(h,m) -> [hitCh h, metCh m]) $ guide
        let options = getCloud (motions (realToFrac targetDiff) seen (met,b))
        (met', nextBeat) <- if not (null options) then Rand.evalRandIO (Rand.weighted options) else pure (met,b)
        when (met' /= met) $ putStrLn "\o33[1;31mNEW TEMPO\o33[0m"
        putStrLn . showBeat . (met',) . fmap (\(h,m) -> [hitCh h, metCh m]) $ superpose nextBeat (Beat met' [()])

        t1 <- do
            let play = fmap (\(h,m) -> metNote m <> silentHitNote h) guide
            let play' | playGuide = fmap (\(h,m) -> metNote m <> hitNote h) guide
                      | otherwise = play
            t1 <- playBeat dest scorer play' t0
            t2 <- playBeat dest scorer play' t1
            t3 <- playBeat dest scorer play t2
            t4 <- playBeat dest scorer play t3
            pure t4

        perf <- perfect scorer
        if perf then
            go dest scorer (Map.insertWith (+) (approxTempo met',nextBeat) 1 seen) (level+1) t1 (met',nextBeat) False
        else
            go dest scorer seen level t1 (met,b) True


    metCh (Just ()) = '|'
    metCh Nothing = '.'

    metNote (Just ()) = (Any False, [56])
    metNote Nothing = mempty

    hitCh (Just True) = '*'
    hitCh (Just False) = '.'
    hitCh Nothing = ' '

    hitNote (Just True) = (Any True, [42])
    hitNote _ = mempty

    silentHitNote (Just True) = (Any True, [])
    silentHitNote _ = mempty
