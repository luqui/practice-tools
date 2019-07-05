{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, TupleSections, ViewPatterns, LambdaCase, ScopedTypeVariables, DeriveGeneric, DeriveAnyClass #-}

import Control.Applicative
import Control.Arrow (first)
import Control.Concurrent (forkIO, threadDelay, myThreadId, killThread)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (throwTo)
import Control.Monad (forM, forM_, void, replicateM, when, forever, join)
import qualified Control.Monad.Logic as Logic
import qualified Control.Monad.Random as Rand
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as State
import Data.Foldable (asum)
import Data.List (nub, sortBy)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Ratio ((%))
import qualified Data.Time.Clock as Clock
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitSuccess))
import qualified System.Posix.Signals as Sig
import qualified Text.Parsec as P

import qualified JSMIDI as MIDI
import qualified Language.Javascript.JSaddle as JS

type Time = Rational

data Phrase a = Phrase Time [(Time, a)]
    deriving (Functor, Show, Generic, NFData)

instance Semigroup (Phrase a) where
    Phrase t es <> Phrase t' es' = Phrase (t+t') (es <> map (first (+t)) es')

instance Monoid (Phrase a) where
    mempty = Phrase 0 []

overlay :: Phrase a -> Phrase a -> Phrase a
overlay (Phrase t es) (Phrase t' es') = Phrase (max t t') (es ++ es')

scale :: Rational -> Phrase a -> Phrase a
scale r (Phrase len evs) = Phrase (len * r) (map (first (*r)) evs)

-- filter out nothings
joinMaybe :: Phrase (Maybe a) -> Phrase a
joinMaybe (Phrase t es) = Phrase t (catMaybes (map sequenceA es))

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
tok p = P.try p <* P.many (void (P.char ' ') <|> comment)

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
        fail $ "Invalid production: label is assigned multiple symbols"
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
    joinMaybe . fmap rendervel <$> renderProduction chooseProd startsym 10
    where
    chooseProd :: String -> Logic.LogicT Cloud Production
    chooseProd label = do
        prods <- lift . weightedShuffle $ [ (prod, prodWeight prod) | prod <- gProductions grammar, prodLabel prod == label, prodFilter prod trackname ]
        asum (map pure prods)


type TrackName = String

data Instrument = Instrument TrackName (Int -> Maybe Note)

instruments :: [Cloud Instrument]
instruments = --[ drumkit [36], drumkit [37], drumkit [38,39], drumkit [40,41], drumkit [42,43,44,45] ]
    [ drumkit "kick" [36], drumkit "snare" [37,38,40], drumkit "hat" [42,44,46], drumkit "tom" [43,45,47], drumkit "ride" [50, 53], drumkit "crash" [55] ]
    where
    drumkit name notes = do
        chosen <- replicateM 5 (Rand.uniform notes)
        pure $ Instrument name $ \i -> if i == 0 then Nothing else Just $ Note 1 (cycle chosen !! i) (min 127 (i * 15 + 30))

jquery :: String -> JS.JSM JS.JSVal
jquery query = do
    jq <- JS.jsg "jQuery"
    JS.call jq jq [query]

loadConfig :: IO (Either P.ParseError Grammar)
loadConfig = do
    Just text <- JS.fromJSVal =<< ((jquery "#drumsheet") JS.# "val") ()
    pure $ P.parse (parseGrammar <* P.eof) "drumsheet" (text ++ "\n")

foldAssoc :: (a -> a -> a) -> a -> [a] -> a
foldAssoc _ z [] = z
foldAssoc _ _ [x] = x
foldAssoc f z xs = foldAssoc f z (pairUp xs)
    where
    pairUp [] = []
    pairUp [x] = [x]
    pairUp (x:y:xs') = f x y : pairUp xs'

main :: IO ()
main = do
    mainThread <- myThreadId
    void $ Sig.installHandler Sig.sigINT (Sig.Catch (throwTo mainThread ExitSuccess)) Nothing

    grammarRef <- newIORef emptyGrammar
    nextPhraseRef <- newIORef mempty
    playThreadIdRef <- newIORef Nothing

    conn <- MIDI.makeOutput "DrumKit"

    let genphrase startsym = do
            grammar <- readIORef grammarRef
            let phraseScale = 60 / fromIntegral (gTempo grammar)
            instrs <- Rand.evalRandIO (sequenceA instruments)
            phrases <- forM instrs $ \instr -> do
                fmap (foldAssoc overlay mempty) . Rand.evalRandIO $ Logic.observeManyT 1 (renderGrammar startsym grammar instr)
            let r = scale phraseScale $ foldAssoc overlay mempty phrases
            rnf r `seq` pure r

    let play = do
            nextPhrase <- readIORef nextPhraseRef
            void . forkIO $ writeIORef nextPhraseRef =<< genphrase "init"
            now <- Clock.getCurrentTime
            playPhrase conn now nextPhrase
            waitUntil (addSeconds 0.1 now)  -- to prevent spamming of emptiness

    --void $ jquery "#drumsheet" JS.# "keyup" $ JS.fun $ \_ _ _ -> abortFail $ do 
    --    grammar <- join $ either (fail.show) pure <$> loadConfig
    --    writeIORef grammarRef grammar

    let playcb = do
            tid <- forkIO $ do
                writeIORef nextPhraseRef =<< genphrase "intro"
                forever play
            writeIORef playThreadIdRef (Just tid)
    let stopcb = do
            tidMay <- readIORef playThreadIdRef
            case tidMay of
                Nothing -> pure ()
                Just tid -> do
                    writeIORef playThreadIdRef Nothing
                    killThread tid
    let reloadcb [successcb, errcb] = do
            grammarE <- loadConfig
            case grammarE of
                Left err -> do
                    void . join $ JS.call errcb <$> JS.jsg "window" <*> pure [show err]
                Right grammar -> do
                    writeIORef grammarRef grammar
                    void . forkIO $ writeIORef nextPhraseRef =<< genphrase "init"
                    void . join $ JS.call successcb <$> JS.jsg "window" <*> pure ()
        reloadcb _ = fail "Wrong number of arguments to reloadcb"
            
    void $ JS.jsg3 "install_handlers" (JS.fun (\_ _ _ -> playcb)) (JS.fun (\_ _ _ -> stopcb)) (JS.fun (\_ _ -> reloadcb))

-- consoleLog :: JS.JSVal -> IO ()
-- consoleLog v = void $ JS.jsg "console" JS.# "log" $ [v]

data Note = Note Int Int Int -- ch note vel
    deriving (Show, Generic, NFData)

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