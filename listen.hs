{-# LANGUAGE MultiWayIf, ScopedTypeVariables #-}

import Prelude hiding (seq)
import qualified System.MIDI as MIDI
import Data.List (isPrefixOf, tails, delete, nub, sortBy, permutations)
import Data.Tuple (swap)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forever, replicateM, join, when, forM_)
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Random as Rand
import Data.Ord (comparing)
import System.IO (hFlush, stdout)

type Cloud = Rand.Rand Rand.StdGen
type Connections = (MIDI.Connection, MIDI.Connection)

getConn :: IO Connections
getConn = do
    dest:_ <- filterM (fmap (== "IAC Bus 1") . MIDI.getName) =<< MIDI.enumerateDestinations
    src:_ <- filterM (fmap (== "Scarlett 18i8 USB") . MIDI.getName) =<< MIDI.enumerateSources
    destconn <- MIDI.openDestination dest
    srcconn <- MIDI.openSource src Nothing
    MIDI.start srcconn
    pure (srcconn,destconn)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

shuffle :: [a] -> Cloud [a]
shuffle [] = pure []
shuffle xs = do
    n <- Rand.uniform (zipWith const [0..] xs)
    ((xs !! n) :) <$> shuffle (take n xs ++ drop (n+1) xs)

choose :: Int -> [a] -> Cloud [a]
choose n xs = take n <$> shuffle xs

increasingExercises :: Cloud [[[Int]]]
increasingExercises = Rand.uniform range >>= \(n0 :: Int) -> go [n0]
    where
    go :: [Int] -> Cloud [[[Int]]]
    go notes = concat <$> sequenceA [ pure [[[head notes]]], goLine notes, goChord notes, (go . (: notes) =<< Rand.uniform (foldr delete range notes)) ]

    goLine :: [Int] -> Cloud [[[Int]]]
    goLine notes = nub . sortBy (comparing length) . deconstruct 3 . map (:[]) <$> shuffle notes

    goChord :: [Int] -> Cloud [[[Int]]]
    goChord notes = map (:[]) <$> choose 4 (combinations 3 notes)
            

    range = [55..67]

    deconstruct :: Int -> [a] -> [[a]]
    deconstruct lmin xs
        | length xs <= lmin = [xs]
        | otherwise = deconstruct lmin pre ++ deconstruct lmin post ++ [xs]
        where
        (pre,post) = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)




noteSequence :: Int -> Cloud Int -> Cloud Int -> Cloud [[Int]]
noteSequence size baseNote intervals = map (:[]) <$> byIntervals size baseNote intervals

fanCloud :: Cloud a -> Cloud a
fanCloud m = Rand.evalRand m <$> Rand.getSplit

main :: IO ()
main = do
    conns <- getConn
    scoredGame conns =<< Rand.evalRandIO (chunkerleave 10 <$> sequenceA (map fanCloud [rowGame, chordGame, intervalGame]))

interleave :: [[a]] -> Cloud [a]
interleave [] = pure []
interleave xss = do
    n <- Rand.uniform $ zipWith const [0..] xss
    case xss !! n of
        [] -> interleave (take n xss ++ drop (n+1) xss)
        (x:xs) -> (x:) <$> interleave (take n xss ++ [xs] ++ drop (n+1) xss)

chunkerleave :: Int -> [[a]] -> [a]
chunkerleave _ [] = []
chunkerleave n ([]:xss) = chunkerleave n xss
chunkerleave n (xs:xss) = take n xs ++ chunkerleave n (xss ++ [drop n xs])

filterCloud :: (a -> Bool) -> Cloud a -> Cloud a
filterCloud p c = do
    x <- c
    if p x
        then pure x
        else filterCloud p c

scale :: Int -> Cloud [Int]
scale baseNote = Rand.uniform . concatMap (\s -> [s, map (subtract 12) (reverse s)]) . map (scanl (+) baseNote) . concat $ 
                    [ take 7 . map (take 7) . tails . cycle $ major, take 7 . map (take 7) . tails . cycle $ melodic ]
    where
    major = [2,2,1,2,2,2,1]
    melodic = [2,1,2,2,2,2,1]

chord :: (Eq a) => Int -> Cloud a -> Cloud [a]
chord 0 _ = pure []
chord n notecloud = do
    note <- notecloud
    (note :) <$> chord (n-1) (filterCloud (/= note) notecloud)


byIntervals :: Int -> Cloud Int -> Cloud Int -> Cloud [Int]
byIntervals notes baseNote intervals = scanl (+) <$> baseNote <*> replicateM (notes-1) intervals
        

sequenceRound :: Connections -> [[Int]] -> IO Bool
sequenceRound (src,dest) chords = do
    let playNotes =
            forM_ chords $ \notes -> do
                forM_ notes $ \note -> MIDI.send dest $ MIDI.MidiMessage 1 (MIDI.NoteOn note 64)
                threadDelay 250000
                forM_ notes $ \note -> MIDI.send dest $ MIDI.MidiMessage 1 (MIDI.NoteOn note 0)

    let expected = reverse $ map Set.fromList chords

    let listenSuccess history win = do
            ch <- listenChord (src,dest)
            if | ch == Set.singleton 108 -> playNotes >> listenSuccess [] (null history)  -- hear again, valid unless you have already started the answer
               | expected `isPrefixOf` (ch:history) -> pure win
               | length (ch:history) < length expected -> listenSuccess (ch:history) win
               | otherwise -> listenSuccess (ch:history) False

    playNotes
    listenSuccess [] True

data ScoreStats = ScoreStats
    { ssScore :: Int
    , ssRunLength :: Int
    , ssDebt :: Map.Map [[Int]] Int
    , ssLives :: Int
    }

scoredGame :: Connections -> [[[Int]]] -> IO ()
scoredGame conns = go (ScoreStats 0 0 Map.empty 4)
    where
    go :: ScoreStats -> [[[Int]]] -> IO ()
    go score exes 
      | ssLives score == 0 = putStrLn "Game Over"
      | otherwise = do
        putStrLn $ "Level: " ++ show (ssScore score) 
              ++ " | Debt: " ++ show (sum (ssDebt score))
              ++ " | Run:  " ++ show (ssRunLength score)
              ++ " | Lives: " ++ show (ssLives score)
        threadDelay 500000

        (round,adv) <- Rand.evalRandIO . Rand.weighted $ ((exes !! ssScore score, True), 5) : [ ((ex, False), fromIntegral w) | (ex,w) <- Map.assocs (ssDebt score), w > 0 ]
        winround <- sequenceRound conns round
        let score' = ScoreStats
                       { ssScore = if winround && adv then ssScore score + 1 else ssScore score
                       , ssRunLength = if winround then ssRunLength score + 1 else 0
                       , ssDebt = if winround then Map.alter (addDebt (-1)) round (ssDebt score)
                                              else Map.alter (addDebt 2) round (ssDebt score)
                       , ssLives = if | winround && (ssRunLength score `mod` 10) == 9 -> ssLives score + 1
                                      | winround -> ssLives score
                                      | otherwise -> ssLives score - 1
                        }
        go score' exes
    
    addDebt x Nothing | x > 0 = Just x
    addDebt x (Just y) | x + y > 0 = Just (x+y)
    addDebt _ _ = Nothing

rowGame :: Cloud [[[Int]]]
rowGame = increasingExercises

scaleGame :: Cloud [[[Int]]]
scaleGame = mapM pickScale [60..]
    where
    pickScale baseNote = do 
        s <- scale baseNote
        s' <- scale (last s)
        pure $ map (:[]) (s <> tail s') 

intervalGame :: Cloud [[[Int]]]
intervalGame = mapM pickPair (concatMap (replicate 3) [50..])
    where
    pickPair range0 = do
        bass <- Rand.uniform [36..48]
        topnote <- Rand.uniform [range0..range0+12]
        pure [[bass, topnote]]

chordGame :: Cloud [[[Int]]]
chordGame = mapM pickChord (concatMap (replicate 3) [50..])
    where
    pickChord range0 = do
        basenote <- Rand.uniform [0..11]
        quality <- Rand.uniform
            [ [ 0, 4, 7 ]  -- maj
            , [ 0, 3, 7 ]  -- min
            , [ 0, 3, 6 ]  -- dim
            , [ 0, 4, 8 ]  -- aug
            , [ 0, 4, 7, 11 ]  -- maj7
            , [ 0, 4, 7, 10 ]  -- 7
            --, [ 0, 4, 6, 11 ]  -- maj7 b5
            --, [ 0, 4, 6, 10 ]  -- 7 b5
            , [ 0, 3, 7, 11 ]  -- maj min
            , [ 0, 3, 7, 11 ]  -- maj min b5
            , [ 0, 3, 7, 10 ]  -- min 7
            --, [ 0, 3, 6, 10 ]  -- min 7 b5
            , [ 0, 3, 6, 9  ]  -- dim7
            --, [ 0, 4, 8, 11 ]  -- maj7 #5
            --, [ 0, 4, 8, 10 ]  -- +7
            ]
        let normalize n = (n `mod` 12) + range0
        pure [map (normalize . (+ basenote)) quality]


listenChord :: Connections -> IO (Set.Set Int)
listenChord (src, dest) = listenOn Set.empty
    where
    listenOn :: Set.Set Int -> IO (Set.Set Int)
    listenOn notes = do
        (note, vel) <- waitNote
        if | vel == 0 -> listenOff notes (Set.delete note notes)
           | otherwise -> listenOn (Set.insert note notes)

    listenOff :: Set.Set Int -> Set.Set Int -> IO (Set.Set Int)
    listenOff notes remnotes
        | Set.null remnotes = pure notes
        | otherwise = do
            (note, vel) <- waitNote
            if | vel == 0 -> listenOff notes (Set.delete note remnotes)
               | otherwise -> listenOff notes remnotes -- ignore note presses after first release (?)

    waitNote :: IO (Int, Int)
    waitNote = do
        e <- getNextEvent (src,dest)
        case e of
            Just (MIDI.MidiEvent _ (MIDI.MidiMessage _ (MIDI.NoteOn note vel))) -> pure (note,vel)
            Just (MIDI.MidiEvent _ (MIDI.MidiMessage _ (MIDI.NoteOff note vel))) -> pure (note,0)
            Nothing -> threadDelay 10000 >> waitNote
            _ -> waitNote

getNextEvent :: Connections -> IO (Maybe (MIDI.MidiEvent))
getNextEvent (src, dest) = do
    e <- MIDI.getNextEvent src
    {-
    case e of
        Just (MIDI.MidiEvent ts msg) -> MIDI.send dest msg
        _ -> pure ()
    -}
    pure e
