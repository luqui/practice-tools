{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf, ScopedTypeVariables, BlockArguments #-}

import Prelude hiding (seq)
import qualified JSMIDI as MIDI
import Data.List (isPrefixOf, tails, delete, nub, sort, sortBy)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Random as Rand
import Data.Ord (comparing)
import qualified Language.Javascript.JSaddle as JS

type Cloud = Rand.Rand Rand.StdGen
type Connections = (MIDI.Input, MIDI.Output)


getConn :: JS.JSM Connections
getConn = MIDI.makeInterface

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

shuffle :: [a] -> Cloud [a]
shuffle [] = pure []
shuffle xs = do
    n <- Rand.uniform (zipWith const [0..] xs)
    ((xs !! n) :) <$> shuffle (take n xs ++ drop (n+1) xs)

choose :: Int -> [a] -> Cloud [a]
choose n xs = take n <$> shuffle xs

rowGame :: Cloud [[[Int]]]
rowGame = do
    baseNote <- Rand.uniform [36..72]
    let range = [baseNote .. baseNote + 12]
    Rand.uniform range >>= \n0 -> go range [n0]
    where
    go range notes 
        | sort notes == range = pure []
        | otherwise =  
            concat <$> sequenceA [ pure [[[head notes]]], goLine notes, goChord notes, (go range . (: notes) =<< Rand.uniform (foldr delete range notes)) ]

    goLine :: [Int] -> Cloud [[[Int]]]
    goLine notes = nub . sortBy (comparing length) . deconstruct 3 . map (:[]) <$> shuffle notes

    goChord :: [Int] -> Cloud [[[Int]]]
    goChord notes = map (:[]) <$> choose 4 (combinations 3 notes)

    deconstruct :: Int -> [a] -> [[a]]
    deconstruct lmin xs
        | length xs <= lmin = [xs]
        | otherwise = deconstruct lmin pre ++ deconstruct lmin post ++ [xs]
        where
        (pre,post) = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)


challenges :: [(String, Connections -> JS.JSM Int)]
challenges = 
    [ "row game" --> game rowGame
    , "tetrachords" --> game chordGame
    , "'melody' and bass" --> game intervalGame
    , "modal scale pairs" --> game scaleGame
    ]
    where
    (-->) = (,)
    game g conns = scoredGame conns =<< evalRandJS g




fanCloud :: Cloud a -> Cloud a
fanCloud m = Rand.evalRand m <$> Rand.getSplit

evalRandJS :: Cloud a -> JS.JSM a
evalRandJS = liftIO . Rand.evalRandIO


jquery :: String -> JS.JSM JS.JSVal
jquery query = do
    jq <- JS.jsg "jQuery"
    JS.call jq jq [query]

main :: JS.JSM ()
main = do
    conns <- getConn
    
    forM_ challenges $ \(name, code) -> do
        let cookiename = name ++ " hiscore"
        hiscore <- fmap (maybe 0 id) . JS.fromJSVal =<< (JS.jsg "jQuery" JS.# "cookie" $ [cookiename])
        button <- jquery "<button>" JS.# "text" $ ["Play"]
        void $ button JS.# "click" $ JS.fun \_ _ _ -> do
            void $ button JS.# "prop" $ ("disabled", True)
            score <- code conns
            void $ button JS.# "prop" $ ("disabled", False)

            when (score > hiscore) $ do
                opts <- JS.eval "{ expires: 365 }"
                void $ JS.jsg "jQuery" JS.# "cookie" $ (cookiename, score, opts)
        jquery "#games" JS.# "append" $ (jquery "<li>" JS.# "text" $ [name ++ " (" ++ show hiscore ++ ")"])
                                                       JS.# "append" $ button

chunkerleave :: Int -> [[a]] -> [a]
chunkerleave _ [] = []
chunkerleave n ([]:xss) = chunkerleave n xss
chunkerleave n (xs:xss) = take n xs ++ chunkerleave n (xss ++ [drop n xs])

scale :: Int -> Cloud [Int]
scale baseNote = Rand.uniform . concatMap (\s -> [s, map (subtract 12) (reverse s)]) . map (scanl (+) baseNote) . concat $ 
                    [ take 7 . map (take 7) . tails . cycle $ major, take 7 . map (take 7) . tails . cycle $ melodic ]
    where
    major = [2,2,1,2,2,2,1]
    melodic = [2,1,2,2,2,2,1]


sequenceRound :: Connections -> [[Int]] -> JS.JSM Bool
sequenceRound (src,dest) chords = do
    let playNotes =
            forM_ chords $ \notes -> do
                forM_ notes $ \note -> MIDI.sendEvent dest $ MIDI.NoteOn 0 note 64
                threadDelay 250000
                forM_ notes $ \note -> MIDI.sendEvent dest $ MIDI.NoteOn 0 note 0

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

showStats :: ScoreStats -> JS.JSM ()
showStats stats = do
    void $ jquery "#level" JS.# "text" $ [show (ssScore stats)]
    void $ jquery "#combo" JS.# "text" $ [show (ssRunLength stats)]
    void $ jquery "#lives" JS.# "text" $ [showlives]
    where
    showlives | ssLives stats == 0 = "Game Over"
              | otherwise = show (ssLives stats)
    

hasIndex :: Int -> [a] -> Bool
hasIndex _ [] = False
hasIndex 0 _ = True
hasIndex n (_:xs) = hasIndex (n-1) xs

scoredGame :: Connections -> [[[Int]]] -> JS.JSM Int
scoredGame conns = go (ScoreStats 0 0 Map.empty 4)
    where
    go score exes 
      | ssLives score == 0 = showStats score >> pure (ssScore score)
      | not (hasIndex (ssScore score) exes) = JS.eval "console.log('Done')" >> showStats score >> pure (ssScore score)
      | otherwise = do
        showStats score
        threadDelay 500000

        (gameround,adv) <- evalRandJS . Rand.weighted $ ((exes !! ssScore score, True), 5) : [ ((ex, False), fromIntegral w) | (ex,w) <- Map.assocs (ssDebt score), w > 0 ]
        winround <- sequenceRound conns gameround
        let score' = ScoreStats
                       { ssScore = if winround && adv then ssScore score + 1 else ssScore score
                       , ssRunLength = if winround then ssRunLength score + 1 else 0
                       , ssDebt = if winround then Map.alter (addDebt (-1)) gameround (ssDebt score)
                                              else Map.alter (addDebt 2) gameround (ssDebt score)
                       , ssLives = if | winround && (ssRunLength score `mod` 10) == 9 -> ssLives score + 1
                                      | winround -> ssLives score
                                      | otherwise -> ssLives score - 1
                       }
        go score' exes
    
    addDebt x Nothing | x > 0 = Just x
    addDebt x (Just y) | x + y > 0 = Just (x+y)
    addDebt _ _ = Nothing

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


listenChord :: Connections -> JS.JSM (Set.Set Int)
listenChord (src, dest) = listenOn Set.empty
    where
    listenOn :: Set.Set Int -> JS.JSM (Set.Set Int)
    listenOn notes = do
        (note, vel) <- waitNote
        if | vel == 0 -> listenOff notes (Set.delete note notes)
           | otherwise -> listenOn (Set.insert note notes)

    listenOff :: Set.Set Int -> Set.Set Int -> JS.JSM (Set.Set Int)
    listenOff notes remnotes
        | Set.null remnotes = pure notes
        | otherwise = do
            (note, vel) <- waitNote
            if | vel == 0 -> listenOff notes (Set.delete note remnotes)
               | otherwise -> listenOff notes remnotes -- ignore note presses after first release (?)

    waitNote :: JS.JSM (Int, Int)
    waitNote = do
        e <- getNextEvent (src,dest)
        case e of
            Just (MIDI.NoteOn _ note vel) -> pure (note,vel)
            Nothing -> threadDelay 10000 >> waitNote

getNextEvent :: Connections -> JS.JSM (Maybe (MIDI.Event))
getNextEvent (src, _) = MIDI.pollEvent src
