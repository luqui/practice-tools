{-# LANGUAGE MultiWayIf #-}

import qualified System.MIDI as MIDI
import qualified Data.Set as Set
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, forever)
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

main :: IO ()
main = do
    conns <- getConn
    singleChordGame conns (chord 1 (Rand.uniform [48..60])) 10
    singleChordGame conns (chord 1 (Rand.uniform [48..72])) 10
    singleChordGame conns (chord 2 (Rand.uniform [48..60])) 10
    singleChordGame conns (chord 2 (Rand.uniform [48..72])) 10
    singleChordGame conns (chord 3 (Rand.uniform [48..60])) 10
    singleChordGame conns (chord 3 (Rand.uniform [48..72])) 10

filterCloud :: (a -> Bool) -> Cloud a -> Cloud a
filterCloud p c = do
    x <- c
    if p x
        then pure x
        else filterCloud p c

chord :: (Eq a) => Int -> Cloud a -> Cloud [a]
chord 0 _ = pure []
chord n notecloud = do
    note <- notecloud
    (note :) <$> chord (n-1) (filterCloud (/= note) notecloud)
        

singleChordRound :: Connections -> Cloud [Int] -> IO Bool
singleChordRound (src,dest) notecloud = do
    notes <- Rand.evalRandIO notecloud

    let playNotes = do
            forM_ notes $ \note -> MIDI.send dest $ MIDI.MidiMessage 1 (MIDI.NoteOn note 64)
            threadDelay 1000000
            forM_ notes $ \note -> MIDI.send dest $ MIDI.MidiMessage 1 (MIDI.NoteOn note 0)

    let listenSuccess win = do
            ch <- listenChord (src,dest)
            if | Set.fromList notes == ch -> pure win
               | ch == Set.singleton 108 -> playNotes >> listenSuccess win
               | otherwise -> listenSuccess False

    playNotes
    listenSuccess True

singleChordGame :: Connections -> Cloud [Int] -> Int -> IO ()
singleChordGame conns chord scoregoal = go 0
    where
    go score | score >= scoregoal = putStrLn "You won the game!"
             | otherwise = do
                putStrLn $ "Score: " ++ show score ++ " / " ++ show scoregoal
                winround <- singleChordRound conns chord
                if winround
                    then go (score+1)
                    else go (max 0 (score-1))

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
