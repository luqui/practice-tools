{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor, TupleSections, ViewPatterns, LambdaCase, ScopedTypeVariables, DeriveGeneric, DeriveAnyClass, BangPatterns #-}

import Control.Applicative
import Control.Arrow (first, (&&&))
import Control.Concurrent (forkIO, threadDelay, myThreadId, killThread)
import Control.DeepSeq (NFData(rnf))
import Control.Exception (throwTo)
import Control.Monad (forM, forM_, void, replicateM, when, join, (<=<))
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
import Data.String (fromString)
import qualified Data.Time.Clock as Clock
import GHC.Generics (Generic)
import System.Exit (ExitCode(ExitSuccess))
import qualified System.Posix.Signals as Sig
import qualified Text.Parsec as P
import Text.Read (readMaybe)

import qualified JSMIDI as MIDI
import qualified Language.Javascript.JSaddle as JS

type Time = Rational

class PhraseLike p where
    phraseLength :: p a -> Time
    overlay :: p a -> p a -> p a
    scale :: Rational -> p a -> p a
    joinMaybe :: p (Maybe a) -> p a

data Phrase a = Phrase Time [(Time, a)]
    deriving (Functor, Show, Generic, NFData)

instance Semigroup (Phrase a) where
    Phrase t es <> Phrase t' es' = Phrase (t+t') (es <> map (first (+t)) es')

instance Monoid (Phrase a) where
    mempty = Phrase 0 []

instance PhraseLike Phrase where
    phraseLength (Phrase t _) = t
    overlay (Phrase t es) (Phrase t' es') = Phrase (max t t') (es ++ es')
    scale r (Phrase len evs) = Phrase (len * r) (map (first (*r)) evs)
    joinMaybe (Phrase t es) = Phrase t (catMaybes (map sequenceA es))

sortPhrase :: Phrase a -> Phrase a
sortPhrase (Phrase len evs) = Phrase len (sortBy (comparing fst) evs)


data AnnoPhrase a = AnnoPhrase (Map.Map String Rational) (Phrase a)
    deriving (Functor, Show, Generic, NFData)

instance Semigroup (AnnoPhrase a) where
    AnnoPhrase e p <> AnnoPhrase e' p' = AnnoPhrase (Map.unionWith (+) e e') (p <> p')

instance Monoid (AnnoPhrase a) where
    mempty = AnnoPhrase Map.empty mempty

instance PhraseLike AnnoPhrase where
    phraseLength (AnnoPhrase _ p) = phraseLength p
    overlay (AnnoPhrase e p) (AnnoPhrase e' p') = AnnoPhrase (Map.unionWith (+) e e') (overlay p p')
    scale s (AnnoPhrase e p) = AnnoPhrase (Map.adjust (/s) "speed" e) (scale s p)
    joinMaybe (AnnoPhrase e p) = AnnoPhrase e (joinMaybe p)

unAnnotate :: AnnoPhrase a -> Phrase a
unAnnotate (AnnoPhrase _ p) = p

addAttrs :: Map.Map String Rational -> AnnoPhrase a -> AnnoPhrase a
addAttrs attrs p = AnnoPhrase attrs mempty <> p

getAttrs :: AnnoPhrase a -> Map.Map String Rational
getAttrs (AnnoPhrase attrs _) = attrs

data Terminal
    = TermVel Int
    | TermRand
    deriving (Show)

data Sym = Sym String String
         | Terminal Terminal
         | Group [Sym]
         | Rescale Sym Rational
         | Pickup Sym
    deriving (Show)
         
data Production = Production 
    { prodFilter :: TrackName -> Bool
    , prodAttrs  :: Map.Map String Rational
    , prodLabel  :: String
    , prodSyms   :: [Sym]
    , prodWeight :: Rational
    }

data Grammar = Grammar
    { gTempo       :: Int   -- allows "4*90",  quarternote 90bpm
    , gAttrs       :: Map.Map String Rational
    , gProductions :: [Production]
    } 

emptyGrammar :: Grammar
emptyGrammar = Grammar (4*100) Map.empty []

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
    attrs <- P.option Map.empty $ tok (P.char '|') *> parseProdAttrs
    when (not . unique . map fst $ nub [ (name, lab) | Sym name lab <- syms ]) $
        fail $ "Invalid production: label is assigned multiple symbols"
    pure $ Production 
        { prodFilter = filt
        , prodAttrs = attrs
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
        , Pickup <$ tok (P.char '\\') <*> parseSym
        ]

    parseName = (:[]) <$> P.oneOf ['A'..'Z']

    parseVel :: Parser Terminal
    parseVel = P.choice
        [ TermVel 0 <$ P.char '.'
        , TermVel 1 <$ P.char '-'
        , TermVel 2 <$ P.char '+'
        , TermVel 3 <$ P.char '*'
        , TermVel 4 <$ P.char '#'
        , TermRand <$ P.char '?'
        ]

    parseLabel :: Parser String
    parseLabel = tok (P.many1 P.alphaNum)

    parseProdAttrs :: Parser (Map.Map String Rational)
    parseProdAttrs = Map.fromList <$> P.many ((,) <$> tok (P.many1 P.alphaNum) <*> eqClause)
        where
        eqClause = P.option 1 $ P.char '=' *> tok parseNumber

comment :: Parser ()
comment = void $ tok (P.string "//") *> P.many (P.satisfy (/= '\n'))

parseNum :: Parser Int
parseNum = product <$> P.sepBy literal (tok (P.string "*"))
    where
    literal = read <$> tok (P.many1 P.digit)

parseNumber :: Parser Rational
parseNumber = P.option id (negate <$ P.string "-") <*> readNum
    where
    readNum = do
        digs <- P.many1 P.digit
        case readMaybe digs of
            Nothing -> fail $ "Bad number: " ++ show digs
            Just n -> pure (fromIntegral (n :: Integer))

parseAttrs :: Parser (Map.Map String Rational)
parseAttrs = fmap Map.fromList . P.many $ tok (P.string "attr " ) *> ((,) <$> tok (P.many1 P.alphaNum) <*> tok parseNumber) <* P.newline

parseGrammar :: Parser Grammar
parseGrammar = do
    tempo <- tok (P.string "tempo ") *> tok parseNum <* P.newline
    attrs <- parseAttrs
    prods <- concat <$> P.many (((:[]) <$> parseProd  <|>  [] <$ tok (pure ())) <* P.newline)
    pure $ Grammar { gTempo = tempo, gAttrs = attrs, gProductions = prods }




type Cloud = Rand.Rand Rand.StdGen

weighted :: [(a, Double)] -> Cloud a
weighted = go 0 (error "weighted: empty list")
    where
    go _ a [] = pure a
    go t a ((x,w):xs) = do
        let !t' = t+w
        r <- Rand.getRandomR (0, t')
        if r <= w
            then go t' x xs
            else go t' a xs

weightedShuffle :: [(a, Rational)] -> Cloud [a]
weightedShuffle [] = pure []
weightedShuffle xs = do
    n <- Rand.weighted (zip [0..] (map snd xs))
    (fst (xs !! n) :) <$> weightedShuffle (take n xs <> drop (n+1) xs)

renderProduction :: (String -> Logic.LogicT Cloud Production) -> String -> Int -> Logic.LogicT Cloud (AnnoPhrase Int)
renderProduction _ _ 0 = empty
renderProduction chooseProd prodname depth = (`State.evalStateT` Map.empty) $ do
    prod <- lift $ chooseProd prodname
    addAttrs (prodAttrs prod) . mconcat <$> traverse renderSym (prodSyms prod)
    where
    baseVPhrase v
        | v == 0 = AnnoPhrase Map.empty $ Phrase 1 [(0,v)]
        | otherwise = AnnoPhrase (Map.fromList [("density", 1), ("volume", fromIntegral v)]) $ Phrase 1 [(0,v)]

    renderSym (Terminal (TermVel v)) = pure $ baseVPhrase v
    renderSym (Terminal TermRand) = lift.lift $ Rand.weighted
        [ (baseVPhrase 0, 25)
        , (baseVPhrase 1, 16)
        , (baseVPhrase 2, 9)
        , (baseVPhrase 3, 4)
        , (baseVPhrase 4, 1)
        ]
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
    renderSym (Pickup sym) = do
        AnnoPhrase e (Phrase t es) <- renderSym sym
        pure $ AnnoPhrase e (Phrase 0 (map (first (subtract t)) es))
                    
renderGrammar :: String -> Grammar -> Instrument -> Logic.LogicT Cloud (AnnoPhrase Note)
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
        pure $ Instrument name $ \i -> if i == 0 then Nothing else Just $ Note 1 (cycle chosen !! i) (min 127 (i * 20 + 30))

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

minmax :: (Ord a, Ord b) => (a,b) -> (a,b) -> (a,b)
minmax (a,z) (a',z') = (min a a', max z z')

main :: IO ()
main = do
    mainThread <- myThreadId
    void $ Sig.installHandler Sig.sigINT (Sig.Catch (throwTo mainThread ExitSuccess)) Nothing

    grammarRef <- newIORef emptyGrammar
    nextPhraseRef <- newIORef mempty
    playThreadIdRef <- newIORef Nothing

    conn <- MIDI.makeOutput =<< JS.jsg "MIDI_INTERFACE"

    let genphrase startsym = do
            grammar <- readIORef grammarRef
            let phraseScale = 60 / fromIntegral (gTempo grammar)
            instrs <- Rand.evalRandIO (sequenceA instruments)
            phrases <- forM instrs $ \instr -> Rand.evalRandIO $ do
                alts <- concat <$> replicateM 50 (Logic.observeManyT 1 (renderGrammar startsym grammar instr))

                let ranges = Map.unionsWith minmax (fmap (\x -> (x,x)) . getAttrs <$> alts)
                let normalize = Map.mapWithKey (\k v -> let (a,z) = ranges Map.! k in if z == a then 0 else (v-a)/(z-a))

                let compWeight (AnnoPhrase attrs _) = 1.1 ** fromRational (sum (Map.intersectionWith (*) (gAttrs grammar) (normalize attrs)))

                if null alts
                    then pure []
                    else pure <$> weighted (map (id &&& compWeight) alts)

            -- is sortPhrase even necessary?
            let r = sortPhrase . scale phraseScale $ foldAssoc overlay mempty (unAnnotate <$> join phrases)
            rnf r `seq` pure r

    let play1 starttime nextPhrase = do
            playPhrase conn starttime nextPhrase
            let phraselen = realToFrac (phraseLength nextPhrase)
            -- to prevent spamming of emptiness
            nexttime <- if phraselen < 0.1 then (consoleLog =<< JS.toJSVal "empty") >> pure (addSeconds 0.1 starttime)
                                           else pure $ addSeconds phraselen starttime
            pure nexttime


    let play starttime = do
            waitUntil starttime
            nextPhrase <- readIORef nextPhraseRef
            void . forkIO $ do
                writeIORef nextPhraseRef =<< genphrase "init"
            play =<< play1 starttime nextPhrase

    api <- JS.create
    setProp' api "play" <=< JS.toJSVal $ JS.fun $ \_ _ _ -> do
        tid <- forkIO $ do
            writeIORef nextPhraseRef =<< genphrase "init"
            intro <- genphrase "intro"
            t <- rnf intro `seq` Clock.getCurrentTime
            play =<< play1 t intro
        writeIORef playThreadIdRef (Just tid)
    setProp' api "stop" <=< JS.toJSVal $ JS.fun $ \_ _ _ -> do
        tidMay <- readIORef playThreadIdRef
        case tidMay of
            Nothing -> pure ()
            Just tid -> do
                writeIORef playThreadIdRef Nothing
                killThread tid
    setProp' api "reload" <=< JS.toJSVal $ JS.fun $ \_ _ [successcb, errcb] -> do
        grammarE <- loadConfig
        case grammarE of
            Left err -> do
                void . join $ JS.call errcb <$> JS.jsg "window" <*> pure [show err]
            Right grammar -> do
                writeIORef grammarRef grammar
                void . forkIO $ writeIORef nextPhraseRef =<< genphrase "init"
                void . join $ JS.call successcb <$> JS.jsg "window" <*> pure ()
    
    void $ JS.jsg1 "install_handlers" api

setProp' :: JS.Object -> String -> JS.JSVal -> JS.JSM ()
setProp' obj prop val = JS.setProp (fromString prop) val obj

consoleLog :: JS.JSVal -> IO ()
consoleLog v = void $ JS.jsg "console" JS.# "log" $ [v]

data Note = Note Int Int Int -- ch note vel
    deriving (Show, Generic, NFData)

addSeconds :: Time -> Clock.UTCTime -> Clock.UTCTime
addSeconds diff base = Clock.addUTCTime (realToFrac diff) base

playPhrase :: MIDI.Output -> Clock.UTCTime -> Phrase Note -> IO ()
playPhrase _ _ (Phrase _ []) = pure ()
playPhrase conn offset (Phrase len evs) = do
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
