{-# OPTIONS -Wall -Wno-type-defaults #-}
{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections, LambdaCase #-}

import qualified System.MIDI as MIDI
import System.Environment (getArgs)
import Control.Monad ((<=<), filterM, forever)
import qualified Data.Map as Map
import Control.Concurrent (yield)
import Control.Concurrent.MVar

openDest :: String -> IO MIDI.Connection
openDest name = do
    cands <- filterM (fmap (== name) . MIDI.getName) =<< MIDI.enumerateDestinations 
    case cands of
        [] -> fail ("No destination named '" ++ name ++ "'")
        cand:_ -> do
            print cand
            MIDI.openDestination cand

openSource :: String -> MIDI.ClientCallback -> IO MIDI.Connection
openSource name cb = do
    cands <- filterM (fmap (== name) . MIDI.getName) =<< MIDI.enumerateSources
    case cands of
        [] -> fail ("No source named '" ++ name ++ "'")
        cand:_ ->  do
            print cand
            conn <- MIDI.openSource cand (Just cb)
            MIDI.start conn
            pure conn


main :: IO ()
main = do
    putStrLn "Sources"
    putStrLn "-------"
    mapM_ (putStrLn <=< MIDI.getName) =<< MIDI.enumerateSources

    putStrLn ""
    putStrLn "Destinations"
    putStrLn "------------"
    mapM_ (putStrLn <=< MIDI.getName) =<< MIDI.enumerateDestinations

    [sourceName, destName] <- getArgs

    schedVar <- newMVar Map.empty
    lastNoteOnVar <- newMVar Map.empty

    dest <- openDest destName
    source <- openSource sourceName $ \(MIDI.MidiEvent ts msg) -> do
        let schedule ts event = modifyMVar_ schedVar (pure . Map.insert ts event)
        case msg of
            MIDI.MidiMessage ch (MIDI.NoteOn n v) -> do
                schedule ts (MIDI.MidiMessage ch (MIDI.NoteOn n v))
                lastNoteOn <- readMVar lastNoteOnVar 
                case Map.lookup n lastNoteOn of
                    Nothing -> pure ()
                    Just ts' -> schedule (ts + (ts - ts')) (MIDI.MidiMessage ch (MIDI.NoteOn n v))
            MIDI.MidiMessage ch (MIDI.NoteOff n v) ->
                modifyMVar_ schedVar (pure . Map.insert ts (MIDI.MidiMessage ch (MIDI.NoteOff n v)))
            m -> MIDI.send dest m

    forever $ do
        window0 <- MIDI.currentTime source
        let window1 = window0 + 1
        sched <- takeMVar schedVar
        case Map.minViewWithKey sched of
            Nothing -> do
                putMVar schedVar sched
                yield

            Just ((ts,e),sched')
                | ts <= window1 -> do
                    putMVar schedVar sched'
                    MIDI.send dest e

                    case e of
                        MIDI.MidiMessage _ (MIDI.NoteOn n v) | v > 0 ->
                            modifyMVar_ lastNoteOnVar (pure . Map.insert n window0)
                        _ -> pure ()
                | otherwise -> do
                    putMVar schedVar sched
                    yield

    _ <- getLine
    MIDI.stop source
    MIDI.close source
    MIDI.close dest

    pure ()

