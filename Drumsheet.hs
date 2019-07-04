{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, TupleSections, ViewPatterns, LambdaCase #-}

import Control.Applicative
import Control.Arrow (first)
import Control.Concurrent (forkIO, threadDelay, myThreadId)
import Control.Exception (throwTo)
import Control.Monad (forM, forM_, void, replicateM, when, forever, join)
import qualified Control.Monad.Logic as Logic
import qualified Control.Monad.Random as Rand
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State
import Data.Foldable (asum)
import Data.List (nub, sortBy)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Data.Ratio ((%))
import System.Exit (ExitCode(ExitSuccess))
import qualified Data.Time.Clock as Clock
import qualified System.Posix.Signals as Sig
import qualified Text.Parsec as P

import qualified JSMIDI as MIDI
import qualified Language.Javascript.JSaddle as JS

type Time = Rational

data Phrase a = Phrase Time [(Time, a)]
    deriving (Functor, Show)

phraseLen :: Phrase a -> Time
phraseLen (Phrase l _) = l

instance Semigroup (Phrase a) where
    Phrase t es <> Phrase t' es' = Phrase (t+t') (es <> map (first (+t)) es')

instance Monoid (Phrase a) where
    mempty = Phrase 0 []

scale :: Rational -> Phrase a -> Phrase a
scale r (Phrase len evs) = Phrase (len * r) (map (first (*r)) evs)

data Sym = Sym String String
         | Terminal Int
         | Group [Sym]
         | Rescale Sym Rational
    deriving (Show)
         
data Production = Production 
    { prodFilter :: TrackName -> Bool
    , prodLabel  :: String
    , prodSyms   :: [Sym]
    , prodWeight :: Rational
    }

data Grammar = Grammar
    { gTempo       :: Int   -- allows "4*90",  quarternote 90bpm
    , gProductions :: [Production]
    } 

emptyGrammar :: Grammar
emptyGrammar = Grammar (4*100) []

type Parser = P.Parsec String ()

tok :: Parser a -> Parser a
tok p = P.try p <* (comment <|> void (P.many (P.char ' ')))

unique :: (Eq a) => [a] -> Bool
unique xs = nub xs == xs

parseProd :: Parser Production
parseProd = do
    filt <- parseFilter
    weight <- parseWeight
    label <- parseLabel
    void $ tok (P.string "=")
    syms <- P.many1 parseSym
    when (not . unique . map fst $ nub [ (name, lab) | Sym name lab <- syms ]) $
        fail "Invalid production: symbol has multiple lengths"
    pure $ Production 
        { prodFilter = filt
        , prodLabel = label
        , prodSyms = syms
        , prodWeight = weight
        }
    where
    parseFilter = P.option (const True) $ 
        tok (P.string "[") *> ((\ts x -> any (x==) ts) <$> P.many parseTrackName) <* tok (P.string "]")
    
    parseTrackName :: Parser TrackName
    parseTrackName = tok $ P.many1 P.alphaNum

    parseWeight :: Parser Rational
    parseWeight = do
        w <- P.option 1 $ tok (P.char '(') *> parseNum <* tok (P.char ')')
        when (w <= 0) $ fail "weights must be positive"
        pure $ fromIntegral w

    parseSym = tok $ P.choice
        [ Sym <$> parseName <* P.char ':' <*> parseLabel
        , Terminal <$> parseVel
        , Group <$ tok (P.char '(') <*> P.many parseSym <* tok (P.char ')')
        , (\a b s -> Rescale s (fromIntegral a % fromIntegral b))
            <$ tok (P.char '[') <*> parseNum <* tok (P.char '/') <*> parseNum <* tok (P.char ']') <*> parseSym
        ]

    parseName = (:[]) <$> P.oneOf ['A'..'Z']

    parseVel :: Parser Int
    parseVel = P.choice
        [ 0 <$ P.char '.'
        , 1 <$ P.char '-'
        , 2 <$ P.char '+'
        , 3 <$ P.char '*'
        , 4 <$ P.char '#'
        ]

    parseLabel :: Parser String
    parseLabel = tok (P.many1 P.alphaNum)

comment :: Parser ()
comment = void $ tok (P.string "//") *> P.many (P.satisfy (/= '\n'))

parseNum :: Parser Int
parseNum = product <$> P.sepBy literal (tok (P.string "*"))
    where
    literal = read <$> tok (P.many1 P.digit)

parseGrammar :: Parser Grammar
parseGrammar = do
    tempo <- tok (P.string "tempo ") *> tok parseNum <* P.newline
    prods <- concat <$> P.many (((:[]) <$> parseProd  <|>  [] <$ tok (pure ())) <* P.newline)
    pure $ Grammar { gTempo = tempo, gProductions = prods }




type Cloud = Rand.Rand Rand.StdGen

weightedShuffle :: [(a, Rational)] -> Cloud [a]
weightedShuffle [] = pure []
weightedShuffle xs = do
    n <- Rand.weighted (zip [0..] (map snd xs))
    map fst . ((xs !! n) :) <$> shuffle (take n xs <> drop (n+1) xs)


shuffle :: [a] -> Cloud [a]
shuffle [] = pure []
shuffle xs = do
    n <- Rand.getRandomR (0, length xs - 1)
    ((xs !! n) :) <$> shuffle (take n xs <> drop (n+1) xs)

renderProduction :: (String -> Logic.LogicT Cloud Production) -> String -> Int -> Logic.LogicT Cloud (Phrase Int)
renderProduction _ _ 0 = empty
renderProduction chooseProd prodname depth = (`State.evalStateT` Map.empty) $ do
    prod <- lift $ chooseProd prodname
    fmap mconcat (traverse renderSym (prodSyms prod))
    where
    renderSym (Terminal v) = pure $ Phrase 1 [(0,v)]
    renderSym (Sym name label) = do
        pad <- State.get
        case Map.lookup name pad of
            Nothing -> do
                rendered <- lift $ renderProduction chooseProd label (depth-1)
                State.put (Map.insert name rendered pad)
                pure rendered
            Just rendered -> pure rendered
    renderSym (Group sym) = fmap mconcat (traverse renderSym sym)
    renderSym (Rescale sym a) = fmap (scale (recip a)) $ renderSym sym
                    
renderGrammar :: String -> Grammar -> Instrument -> Logic.LogicT Cloud (Phrase Note)
renderGrammar startsym grammar (Instrument trackname rendervel) = do
    fmap rendervel <$> renderProduction chooseProd startsym 10
    where
    chooseProd :: String -> Logic.LogicT Cloud Production
    chooseProd label = do
        prods <- lift . weightedShuffle $ [ (prod, prodWeight prod) | prod <- gProductions grammar, prodLabel prod == label, prodFilter prod trackname ]
        asum (map pure prods)


type TrackName = String

data Instrument = Instrument TrackName (Int -> Note)

instruments :: [Cloud Instrument]
instruments = --[ drumkit [36], drumkit [37], drumkit [38,39], drumkit [40,41], drumkit [42,43,44,45] ]
    [ drumkit "kick" [36], drumkit "snare" [37,38,39,40], drumkit "hat" [42,44,46], drumkit "tom" [41,43,45,47], drumkit "ride" [50, 53], drumkit "crash" [55] ]
    where
    drumkit name notes = do
        chosen <- replicateM 5 (Rand.uniform notes)
        pure $ Instrument name $ \i -> Note 1 (cycle chosen !! i) (if i == 0 then 0 else min 127 (i * 15 + 30))

jquery :: String -> JS.JSM JS.JSVal
jquery query = do
    jq <- JS.jsg "jQuery"
    JS.call jq jq [query]

loadConfig :: IO (Either P.ParseError Grammar)
loadConfig = do
    Just text <- JS.fromJSVal =<< ((jquery "#drumsheet") JS.# "val") ()
    void $ JS.jsg "console" JS.# "log" $ [text]
    pure $ P.parse (parseGrammar <* P.eof) "<textarea>" (text ++ "\n")

main :: IO ()
main = do
    mainThread <- myThreadId
    void $ Sig.installHandler Sig.sigINT (Sig.Catch (throwTo mainThread ExitSuccess)) Nothing

    grammarRef <- newIORef emptyGrammar

    (_, conn) <- MIDI.makeInterface

    let play startsym = do
            grammar <- readIORef grammarRef
            let phraseScale = 60 / fromIntegral (gTempo grammar)
            instrs <- Rand.evalRandIO (sequenceA instruments)

            now <- Clock.getCurrentTime
            lens <- forM instrs $ \instr -> do
                phraseMay <- Rand.evalRandIO $ Logic.observeManyT 1 (renderGrammar startsym grammar instr)
                case phraseMay of
                    [] -> pure 0
                    phrase:_ -> do
                        void . forkIO . playPhrase conn now $ scale phraseScale phrase
                        pure (phraseLen phrase)
            waitUntil (addSeconds (phraseScale * max (maximum lens) 0.1) now)

    void $ jquery "#run" JS.# "click" $ JS.fun $ \_ _ _ -> do
        grammar0 <- join $ either (fail.show) pure <$> loadConfig
        writeIORef grammarRef grammar0
        play "intro"
        forever (play "init")


data Note = Note Int Int Int -- ch note vel
    deriving (Show)

addSeconds :: Time -> Clock.UTCTime -> Clock.UTCTime
addSeconds diff base = Clock.addUTCTime (realToFrac diff) base

playPhrase :: MIDI.Output -> Clock.UTCTime -> Phrase Note -> IO ()
playPhrase _ _ (Phrase _ []) = pure ()
playPhrase conn offset (Phrase len (sortBy (comparing fst) -> evs)) = do
    forM_ (zip evs (map fst (tail evs) ++ [len])) $ \((t,Note ch note vel),t') -> do
        waitUntil (addSeconds t offset)
        MIDI.sendEvent conn (MIDI.NoteOn ch note vel)
        waitUntil (addSeconds t' offset)
        MIDI.sendEvent conn (MIDI.NoteOn ch note 0)

waitUntil :: Clock.UTCTime -> IO ()
waitUntil target = do
    now <- Clock.getCurrentTime
    let delay = Clock.diffUTCTime target now
    when (delay > 0) (threadDelay (round (1000000 * delay)))
