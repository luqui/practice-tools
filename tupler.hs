{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections #-}

import qualified System.MIDI as MIDI
import Control.Monad (filterM, guard, forM_, ap, join, (<=<), when)
import Data.Ratio
import Data.List (inits, tails, genericLength, genericReplicate, transpose)
import qualified Control.Monad.Random as Rand
import qualified Data.Time.Clock.POSIX as Clock
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Map as Map
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
    , denominator s ^ (2::Int)
    , denominator s' ^ (2::Int)
    ]
    where
    Beat s' h' = superpose (Beat s h) (Beat met [()])

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
    go dest Map.empty now (1, Beat 1 [True])
    where
    go :: MIDI.Connection -> Map.Map (Rational, Beat Bool) Integer -> Clock.POSIXTime -> (Rational, Beat Bool) -> IO ()
    go dest seen t0 (met, b) = do
        putStrLn "\o33[2J\o33[1;1f"
        putStrLn $ "\o33[1;33m" ++ show (beatWeight (met,b)) ++ " points\o33[0m\n"
        let guide = superpose b (Beat met [()])
        putStrLn . showBeat . (met,) . fmap (\(h,m) -> [hitCh h, metCh m]) $ guide
        let options = getCloud (motions seen (met,b))
        (met', nextBeat) <- if not (null options) then Rand.evalRandIO (Rand.weighted options) else pure (met,b)
        when (met' /= met) $ putStrLn "\o33[1;31mNEW TEMPO\o33[0m"
        putStrLn . showBeat . (met',) . fmap (\(h,m) -> [hitCh h, metCh m]) $ superpose nextBeat (Beat met' [()])

        t1 <- do
            -- let play = fmap (\(_,m) -> metNote m) guide
            let play' = fmap (\(h,m) -> metNote m ++ hitNote h) guide
            t1 <- playBeat dest play' t0
            t2 <- playBeat dest play' t1
            t3 <- playBeat dest play' t2
            t4 <- playBeat dest play' t3
            pure t4
        go dest (Map.insertWith (+) (met',nextBeat) 1 seen) t1 (met',nextBeat)

    metCh (Just ()) = '|'
    metCh Nothing = '.'

    metNote (Just ()) = [42]
    metNote Nothing = []

    hitCh (Just True) = '*'
    hitCh (Just False) = '.'
    hitCh Nothing = ' '

    hitNote (Just True) = [37]
    hitNote _ = []
