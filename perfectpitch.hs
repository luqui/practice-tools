import qualified System.MIDI as MIDI
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (filterM, forever)
import Control.Monad.Random as Rand
import System.IO (hFlush, stdout)

type Cloud = Rand.Rand Rand.StdGen

getConn :: IO MIDI.Connection
getConn = do
    dest:_ <- filterM (fmap (== "IAC Bus 1") . MIDI.getName) 
                    =<< MIDI.enumerateDestinations 
    conn <- MIDI.openDestination dest
    MIDI.start conn
    return conn

main :: IO ()
main = do
    conn <- getConn

    forever $ gameRound conn

randomChord :: Cloud [Int]
randomChord = do
    baseNote <- Rand.uniform [0..11]
    let notes = [baseNote, baseNote+4, baseNote+7]
    return $ map ((48 +) . (`mod` 12)) notes

random251 :: Cloud [[Int]]
random251 = do
    baseNote <- Rand.uniform [0..11]
    let noteses = [ [baseNote+2, baseNote+5, baseNote+9]
                  , [baseNote+2, baseNote+5, baseNote+7, baseNote+11]
                  , [baseNote, baseNote+4, baseNote+7]
                  ]
    return $ (map.map) ((60+) . (`mod` 12)) noteses


playNote :: MIDI.Connection -> Int -> Int -> Double -> IO ()
playNote conn note vel dur = void . forkIO $ do
    MIDI.send conn $ MIDI.MidiMessage 1 (MIDI.NoteOn note vel)
    threadDelay (round (10^6 * dur))
    MIDI.send conn $ MIDI.MidiMessage 1 (MIDI.NoteOn note 0)

playMask :: MIDI.Connection -> IO ()
playMask conn = do
    replicateM_ 4 $ do
        chords <- Rand.evalRandIO random251
        forM_ chords $ \chord -> do
            forM_ chord $ \note -> do
                playNote conn note 64 0.25
            threadDelay 300000

        replicateM_ 3 $ do
            note <- Rand.evalRandIO $ Rand.uniform [48..72]
            playNote conn note 64 0.25
            threadDelay 300000
        threadDelay 300000

gameRound :: MIDI.Connection -> IO ()
gameRound conn = do
    note <- Rand.evalRandIO $ Rand.uniform [0..11]
    octave <- Rand.evalRandIO $ Rand.uniform [36,48,60,72]
    noteRound note octave
    where
    noteRound note octave = do
        playNote conn (note+octave) 64 1.0
        putStr "Guess? "
        hFlush stdout
        guess <- getLine
        case parseNote guess of
            Just g 
              | g == note -> do
                putStrLn "Yes!"
                MIDI.send conn $ MIDI.MidiMessage 1 (MIDI.NoteOn (note+octave) 64)
                putStrLn "Press enter for next round"
                getLine
                MIDI.send conn $ MIDI.MidiMessage 1 (MIDI.NoteOn (note+octave) 0)
                playMask conn
              | otherwise -> do
                putStrLn "No!"
                noteRound note octave
            Nothing -> putStrLn "What?" >> noteRound note octave
    

parseNote :: String -> Maybe Int
parseNote "C"  = Just 0
parseNote "C#" = Just 1
parseNote "Db" = Just 1
parseNote "D"  = Just 2
parseNote "D#" = Just 3
parseNote "Eb" = Just 3
parseNote "E"  = Just 4
parseNote "F"  = Just 5
parseNote "F#" = Just 6
parseNote "Gb" = Just 6
parseNote "G"  = Just 7
parseNote "G#" = Just 8
parseNote "Ab" = Just 8
parseNote "A"  = Just 9
parseNote "A#" = Just 10
parseNote "Bb" = Just 10
parseNote "B"  = Just 11
parseNote _    = Nothing
