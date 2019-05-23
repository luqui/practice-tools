{-# LANGUAGE MultiWayIf #-}

import Prelude hiding (seq)
import qualified System.MIDI as MIDI
import Data.List (isPrefixOf, tails)
import qualified Data.Set as Set
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forever, replicateM, join, when)
import Control.Monad.Random as Rand
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

getExercise :: Int -> Cloud [[Int]]
getExercise score = join $ Rand.weighted [ (cloud, 1 / fromIntegral (abs (score-level) + 1)) | (level, cloud) <- exercises, score >= level]
    where
    exercises :: [(Int, Cloud [[Int]])]
    exercises = zip [0,5..]
        [ seq 1   $ chord 1 (Rand.uniform range)
        , seq 2   $ seq 1 $ Rand.uniform range
        , noteSequence 2 (Rand.uniform range) (Rand.uniform [-2,-1,1,2])
        , seq 1   $ chord 2 $ Rand.uniform range
        , noteSequence 4 (Rand.uniform range) (Rand.uniform [-3,-2,-1,1,2,3])
        , seq 1   $ chord 2 $ Rand.uniform range
        , noteSequence 3 (Rand.uniform range) (Rand.uniform [-4,-3,-2,-1,1,2,3,4])
        , seq 1 $ byIntervals 3 (Rand.uniform range) (Rand.uniform [3,4])
        , seq 1 $ byIntervals 3 (Rand.uniform range) (Rand.uniform [3,4,5])
        , noteSequence 4 (Rand.uniform range) (Rand.uniform [-4,-3,-2,-1,1,2,3,4])
        , seq 1 $ byIntervals 3 (Rand.uniform range) (Rand.uniform [2,3,4,5])
        , seq 1 $ byIntervals 3 (Rand.uniform range) (Rand.uniform [2,3,4,5,7])
        , noteSequence 4 (Rand.uniform range) (Rand.uniform [-5,-4,-3,-2,-1,1,2,3,4,5])
        , seq 1 $ byIntervals 3 (Rand.uniform range) (Rand.uniform [2,3,4,5,6,7])
        , seq 1 $ byIntervals 3 (Rand.uniform range) (Rand.uniform [2,3,4,5,6,7,8,9])
        , seq 1 $ byIntervals 3 (Rand.uniform range) (Rand.uniform [1,2,3,4,5,6,7,8,9])
        , noteSequence 5 (Rand.uniform range) (Rand.uniform [-5,-4,-3,-2,-1,1,2,3,4,5])
        , noteSequence 5 (Rand.uniform range) (Rand.uniform [-6,-5,-4,-3,-2,-1,1,2,3,4,5,6])
        , noteSequence 6 (Rand.uniform range) (Rand.uniform [-6,-5,-4,-3,-2,-1,1,2,3,4,5,6])
        , noteSequence 6 (Rand.uniform range) (Rand.uniform [-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7])
        , noteSequence 7 (Rand.uniform range) (Rand.uniform [-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7])
        , noteSequence 7 (Rand.uniform range) (Rand.uniform [-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8])
        , noteSequence 8 (Rand.uniform range) (Rand.uniform [-8,-7,-6,-5,-4,-3,-2,-1,1,2,3,4,5,6,7,8])
        
        ]

    infix 0 -->
    (-->) = (,)

    seq = replicateM

    range = [55..67]

noteSequence :: Int -> Cloud Int -> Cloud Int -> Cloud [[Int]]
noteSequence size baseNote intervals = map (:[]) <$> byIntervals size baseNote intervals


main :: IO ()
main = do
    conns <- getConn
    --chordGame conns
    scaleGame conns

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
            if | ch == Set.singleton 108 -> playNotes >> listenSuccess history win  -- hear again
               | expected `isPrefixOf` (ch:history) -> pure win
               | length (ch:history) < length expected -> listenSuccess (ch:history) win
               | otherwise -> listenSuccess (ch:history) False

    playNotes
    listenSuccess [] True

chordGame :: Connections -> IO ()
chordGame conns = go 0
    where
    go score = do
        putStrLn $ "Score: " ++ show score 
        threadDelay 500000
        winround <- sequenceRound conns =<< Rand.evalRandIO (getExercise score)
        if winround
            then go (score+1)
            else go (max 0 (score-1))

scaleGame :: Connections -> IO ()
scaleGame conns = go 60
    where
    go baseNote = do
        threadDelay 500000
        thescale <- Rand.evalRandIO $ do { s <- scale baseNote; s' <- scale (last s); pure (s <> tail s') }
        if (last thescale >= 36 && last thescale <= 96) then do
            void (sequenceRound conns (map (:[]) thescale))
            go (last thescale)
        else
            go baseNote

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
    case e of
        Just (MIDI.MidiEvent ts msg) -> MIDI.send dest msg
        _ -> pure ()
    pure e