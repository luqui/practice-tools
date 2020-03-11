{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections #-}

import qualified System.MIDI as MIDI
import Control.Monad (filterM, guard, forM_, ap, join)
import Data.Ratio
import Data.List (inits, tails, genericLength, genericReplicate, transpose)
import qualified Control.Monad.Random as Rand
import qualified Data.Time.Clock.POSIX as Clock
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Set as Set
import Data.Tuple (swap)

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
puniform = Cloud . map (,1)

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

showBeat :: Beat [Char] -> String
showBeat beat = unlines (meter : transpose (bHits beat))
    where
    meter = show (numerator (bSubdiv beat)) ++ "/" ++ show (denominator (bSubdiv beat))

motions :: Set.Set (Beat Bool) -> Beat Bool -> Cloud (Beat Bool)
motions seen (Beat subdiv hits)  = pfilter valid . fmap (\(Beat s h) -> Beat s (cycleGenerator h)) . join . puniform $
    [ dup, slice, addOrRemove, halfTime, doubleTime, prune, split, trim ]
    where
    dup = Beat subdiv <$> puniform [hits, hits ++ hits, hits ++ hits ++ hits ]  -- breaking invariant briefly...
    slice = puniform $ do
        offs <- [-2..2]
        let len = offs + length hits
        guard $ len > 1
        pure $ mkBeat subdiv (take len (cycle hits))
    addOrRemove = puniform $ do
        (pre, x:post) <- zip (inits hits) (tails hits)
        let hits' = pre ++ not x : post
        guard $ any id hits'
        pure $ mkBeat subdiv hits'
    halfTime = pure $ mkBeat (subdiv/2) hits
    doubleTime = pure $ mkBeat (subdiv*2) hits
    split = puniform $ do
        d <- [2,3,5]
        pure $ mkBeat (subdiv/fromIntegral d) (concat [ h : replicate (d-1) False | h <- hits ])
    prune = puniform $ do
        d <- [2,3]
        o <- [0..d-1]
        let guide = replicate o False ++ cycle (True : replicate (d-1) False)
        guard . and $ zipWith (\a b -> a || not b) guide hits
        pure $ mkBeat (fromIntegral d * subdiv) (map snd . filter fst $ zip guide hits)
    trim = puniform $ do
        (pre, _:post) <- tail . init $ zip (inits hits) (tails hits)
        pure $ mkBeat subdiv (pre ++ post)

    valid b@(Beat s h) = and
        [ b /= Beat subdiv hits
        , s' <= 1
        , s < 1
        , or h
        , length h' <= 80
        , b `Set.notMember` seen
        ]
        where
        Beat s' h' = superpose b (Beat 1 [()])

divisors :: (Integral a) => a -> [a]
divisors n = [ m | m <- [1..n], n `mod` m == 0 ]

playBeat :: MIDI.Connection -> Beat [Int] -> Clock.POSIXTime -> IO Clock.POSIXTime
playBeat dest beat t0 = do
    let subdiv = realToFrac (bSubdiv beat)
    waitUntil t0
    _ <- forkIO $ do
        forM_ (zip [t0, t0 + subdiv..] (bHits beat)) $ \(t,notes) -> do
            forM_ notes $ \n -> MIDI.send dest (MIDI.MidiMessage 1 (MIDI.NoteOn n 100))
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


main :: IO ()
main = do
    dest <- openDest "IAC Bus 1"
    now <- Clock.getPOSIXTime
    go dest Set.empty now (Beat 1 [True])
    where
    go :: MIDI.Connection -> Set.Set (Beat Bool) -> Clock.POSIXTime -> Beat Bool -> IO ()
    go dest seen t0 b = do
        putStrLn "\o33[2J\o33[1;1f"
        let guide = superpose b (Beat 1 [()])
        putStrLn . showBeat . fmap (\(h,met) -> [hitCh h, metCh met]) $ guide
        let options = getCloud (motions seen b)
        nextBeat <- if not (null options) then Rand.evalRandIO (Rand.weighted options) else pure b
        putStrLn . showBeat . fmap (\(h,met) -> [hitCh h, metCh met]) $ superpose nextBeat (Beat 1 [()])

        t1 <- do
            let play = fmap (\(_,met) -> metNote met) guide
            let play' = fmap (\(h,met) -> metNote met ++ hitNote h) guide
            t1 <- playBeat dest play t0
            t2 <- playBeat dest play t1
            t3 <- playBeat dest play' t2
            t4 <- playBeat dest play' t3
            pure t4
        go dest (Set.insert nextBeat seen) t1 nextBeat

    metCh (Just ()) = '|'
    metCh Nothing = '.'

    metNote (Just ()) = [42]
    metNote Nothing = []

    hitCh (Just True) = '*'
    hitCh (Just False) = '.'
    hitCh Nothing = ' '

    hitNote (Just True) = [37]
    hitNote _ = []
