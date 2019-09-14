{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments #-}
module JSMIDI (makeInput, makeOutput, makeInterface, Event(..), Input(..), Output(..)) where

import Prelude
import Control.Arrow (first)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), (.|.))
import Data.IORef
import Data.Tuple (swap)
import qualified Deque.Strict as Deque
import qualified Language.Javascript.JSaddle as JS

newtype Input = Input { pollEvent :: JS.JSM (Maybe Event) }

newtype Output = Output { sendEvent :: Event -> JS.JSM () }


makeOutput :: JS.JSVal -> JS.JSM Output
makeOutput outobj = do
    let send e = do
            dat <- toJSData e
            void $ outobj JS.# "send" $ dat
    pure (Output send)

makeInput :: JS.JSVal -> JS.JSM Input
makeInput = fmap Input . makeMIDIPoller 

makeInterface :: JS.JSVal -> JS.JSM (Input, Output)
makeInterface obj = (,) <$> makeInput obj <*> makeOutput obj

data Event = NoteOn Int Int Int  -- channel (0 based), key, vel normalized with NoteOff = NoteOn 0
    deriving (Show)

fromJSData :: JS.JSVal -> JS.JSM (Maybe Event)
fromJSData dat = do
    d0 <- JS.fromJSValUnchecked =<< dat JS.!! 0
    d1 <- JS.fromJSValUnchecked =<< dat JS.!! 1
    d2 <- JS.fromJSValUnchecked =<< dat JS.!! 2
    let ch = d0 .&. 0x0f
    pure $ case d0 .&. 0xf0 of
        0x80 -> Just $ NoteOn ch d1 0
        0x90 -> Just $ NoteOn ch d1 d2
        _ -> Nothing
    
toJSData :: Event -> JS.JSM JS.JSVal
toJSData (NoteOn ch k v) = JS.toJSVal [0x90 .|. ch, k, v]

makeMIDIPoller :: JS.JSVal -> JS.JSM (JS.JSM (Maybe Event))
makeMIDIPoller interface = do
    queue <- liftIO $ newIORef mempty
    void $ (interface JS.! "event") JS.# "listen" $ JS.fun \_ _ [e] -> do
        maybee' <- fromJSData e
        forM_ maybee' $ liftIO . modifyIORef queue . Deque.snoc
    pure . liftIO $ atomicModifyIORef queue (\q -> swap (maybe (Nothing, q) (first Just) (Deque.uncons q)))

