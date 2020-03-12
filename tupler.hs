{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections #-}

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

motions :: Map.Map (Rational, Beat Bool) Integer -> (Rational, Beat Bool) -> Cloud (Rational, Beat Bool)
motions seen (met, Beat subdiv hits)  = weightify <=< pfilter valid . join . puniform $
    [ dup, slice, addOrRemove, halfTime, doubleTime, remeter, prune, rot, split, trim ]
    where
    dup = (met,) . Beat subdiv <$> puniform [hits, hits ++ hits, hits ++ hits ++ hits ]  -- breaking invariant briefly...
    slice = fmap (met,) . puniform $ do
        offs <- [-2..2]
        let len = offs + length hits
        guard $ len > 1
        pure $ mkBeat subdiv (take len (cycle hits))
    addOrRemove = fmap (met,) . puniform $ do
        (pre, x:post) <- zip (inits hits) (tails hits)
        let hits' = pre ++ not x : post
        guard $ any id hits'
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

    weightify b = Cloud [(b, 1 % (beatWeight b + 10 * Map.findWithDefault 0 b seen)^(2::Int))]

    valid (m, Beat s h) = and
        [ s' <= 1
        , s < 1
        , or h
        , length h' <= 80
        , 1/2 <= m && m <= 2
        ]
        where
        Beat s' h' = superpose (Beat s h) (Beat m [()])

beatWeight :: (Rational, Beat Bool) -> Integer
beatWeight (met, Beat s h) = sum
    [ 4 * genericLength h
    , genericLength h'
    , denominator (s/met) ^ (2::Int)
    , denominator (s'/met) ^ (2::Int)
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
    }

makeScorer :: MIDI.Connection -> IO Scorer
makeScorer dest = do
    stateRef <- newIORef (Nothing, Nothing, 100)
    
    let accuracy :: Clock.POSIXTime -> Clock.POSIXTime -> Double
        accuracy t t' 
            = let r = 2 * exp ( -(15*realToFrac (t - t'))^(2::Int) ) - 1 
              in if | r < 0 -> 10*r
                    | r > 0.9 -> r - 2 * log (1 - r) / log 10
                    | otherwise -> r
    let report :: Double -> IO ()
        report diff 
            | diff > 1 = putStr $ "\o33[1G\o33[K\o33[1;32m+" ++ show diff ++ "\o33[0m"
            | diff > -1 = putStr $ "\o33[1G\o33[K\o33[1;33m+" ++ show diff ++ "\o33[0m"
            | otherwise = putStr $ "\o33[1G\o33[K\o33[1;31m" ++ show diff ++ "\o33[0m"

    let onMet' t = do
            state <- readIORef stateRef
            state' <- case state of
                        (Nothing, Just h, score)
                            | accuracy t h > 0 -> report (accuracy t h) >> pure (Nothing, Nothing, score + accuracy t h)
                            | otherwise -> report (accuracy t h) >> pure (Just t, Nothing, score + accuracy t h)
                        (Nothing, Nothing, score)
                            -> pure (Just t, Nothing, score)
                        (Just _t', Nothing, score)
                            -> report (-10) >> pure (Just t, Nothing, score - 10)
                        (Just _t', Just _h', _score) -> error "Impossible!"
            writeIORef stateRef state'
    
    let onHit' h = do
            state <- readIORef stateRef
            state' <- case state of
                        (Just t, Nothing, score)
                            | accuracy t h > 0 -> report (accuracy t h) >> pure (Nothing, Nothing, score + accuracy t h)
                            | otherwise -> report (accuracy t h) >> pure (Nothing, Just h, score + accuracy t h)
                        (Nothing, Nothing, score)
                            -> pure (Nothing, Just h, score)
                        (Nothing, Just _h', score)
                            -> report (-10) >> pure (Nothing, Just h, score - 10)
                        (Just _t', Just _h', _score) -> error "Impossible!"
            writeIORef stateRef state'

    _ <- forkIO $ do
        IO.hSetBuffering IO.stdin IO.NoBuffering
        forever $ do
            _ <- getChar
            MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn 37 100))
            MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn 37 0))
            onHit' =<< Clock.getPOSIXTime

    pure $ Scorer onMet' onHit' ((\(_,_,s) -> s) <$> readIORef stateRef)

main :: IO ()
main = do
    dest <- openDest "IAC Bus 1"
    now <- Clock.getPOSIXTime
    scorer <- makeScorer dest
    tempo0 <- Rand.evalRandIO $ Rand.uniform [50/100, 51/100.. 200/100]
    time0 <- playBeat dest scorer (Beat tempo0 (replicate 4 (Any False, [56]))) now
    go dest scorer now Map.empty time0 (tempo0, Beat tempo0 [True])
    where
    go :: MIDI.Connection -> Scorer -> Clock.POSIXTime -> Map.Map (Rational, Beat Bool) Integer -> Clock.POSIXTime -> (Rational, Beat Bool) -> IO ()
    go dest scorer gameStart seen t0 (met, b) = do
        putStrLn "\o33[2J\o33[1;1f"
        health <- getScore scorer
        score <- floor . subtract gameStart <$> Clock.getPOSIXTime :: IO Integer
        putStrLn $ "\o33[1;32mScore: " ++ show score ++ "\o33[0m"
        putStrLn $ "\o33[1;33mHealth: " ++ show health ++ "\o33[0m\n"
        when (health < 0) (putStrLn "GAME OVER" >> exitSuccess)
        let guide = superpose b (Beat met [()])
        putStrLn . showBeat . (met,) . fmap (\(h,m) -> [hitCh h, metCh m]) $ guide
        let options = getCloud (motions seen (met,b))
        (met', nextBeat) <- if not (null options) then Rand.evalRandIO (Rand.weighted options) else pure (met,b)
        when (met' /= met) $ putStrLn "\o33[1;31mNEW TEMPO\o33[0m"
        putStrLn . showBeat . (met',) . fmap (\(h,m) -> [hitCh h, metCh m]) $ superpose nextBeat (Beat met' [()])

        t1 <- do
            let play = fmap (\(h,m) -> metNote m <> silentHitNote h) guide
            let play' = fmap (\(h,m) -> metNote m <> hitNote h) guide
            t1 <- playBeat dest scorer play t0
            t2 <- playBeat dest scorer play t1
            t3 <- playBeat dest scorer play' t2
            t4 <- playBeat dest scorer play' t3
            pure t4
        go dest scorer gameStart (Map.insertWith (+) (met',nextBeat) 1 seen) t1 (met',nextBeat)

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
