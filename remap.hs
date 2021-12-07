{-# OPTIONS -Wall -Wno-type-defaults #-}
{-# LANGUAGE MultiWayIf, DeriveFunctor, TupleSections, LambdaCase #-}

import qualified System.MIDI as MIDI
import System.Environment (getArgs)
import Control.Monad ((<=<), filterM)

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

noteMap :: Int -> Int
-- noteMap n = 127 - n

{-noteMap n = 12 * oct + ((7*deg) `mod` 12)
    where
    (oct,deg) = n `divMod` 12
-}
{-
noteMap n = 12 * oct + ((2^deg) `mod` 13 - 1)
    where
    (oct,deg) = n `divMod` 12
-}
noteMap n = 12 * oct + swap deg
    where
    (oct,deg) = n `divMod` 12
    swap 0 = 0
    swap 1 = 1
    swap 2 = 2
    swap 3 = 4
    swap 4 = 3
    swap 5 = 5
    swap 6 = 6
    swap 7 = 7
    swap 8 = 9
    swap 9 = 8
    swap 10 = 11
    swap 11 = 10



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

    dest <- openDest destName
    source <- openSource sourceName $ \(MIDI.MidiEvent _ts msg) ->
        case msg of
            MIDI.MidiMessage ch (MIDI.NoteOn n v) -> 
                MIDI.send dest (MIDI.MidiMessage ch (MIDI.NoteOn (noteMap n) v))
            MIDI.MidiMessage ch (MIDI.NoteOff n v) ->
                MIDI.send dest (MIDI.MidiMessage ch (MIDI.NoteOff (noteMap n) v))
            m -> MIDI.send dest m

    _ <- getLine
    MIDI.stop source
    MIDI.close source
    MIDI.close dest
    pure ()
