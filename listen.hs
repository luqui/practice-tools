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
    forever $ gameRound conns >> threadDelay 1000000

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
        

gameRound :: Connections -> IO ()
gameRound (src, dest) = do
    notes <- Rand.evalRandIO (chord 2 (Rand.uniform [48..60]))

    let playNotes = do
            forM_ notes $ \note -> MIDI.send dest $ MIDI.MidiMessage 1 (MIDI.NoteOn note 64)
            threadDelay 1000000
            forM_ notes $ \note -> MIDI.send dest $ MIDI.MidiMessage 1 (MIDI.NoteOn note 0)

    let listenSuccess = do
            ch <- listenChord (src,dest)
            if | Set.fromList notes == ch -> putStrLn "Yes!"
               | ch == Set.singleton 108 -> playNotes >> listenSuccess
               | otherwise -> putStrLn "Nope" >> listenSuccess

    playNotes
    listenSuccess
    

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
