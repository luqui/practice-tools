{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments #-}
module JSMIDI where

import Prelude hiding ((!!))
import Control.Arrow (first)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), (.|.))
import Data.IORef
import Data.Tuple (swap)
import qualified Deque.Strict as Deque
import Language.Javascript.JSaddle

newtype Input = Input { pollEvent :: JSM (Maybe Event) }

newtype Output = Output { sendEvent :: Event -> JSM () }


makeOutput :: String -> JSM Output
makeOutput synthmod = do
    tools <- jsgf "MIDITools" (
        jsg "jQuery",
        jsg1 "Keyboard" =<< jsg "jQuery")

    -- XXX hacky DI design
    makesynth <- eval $ "() => new (" ++ synthmod ++ "(Tone)).Synth()"
    
    outp <- new (tools ! "OutputSelector") (makesynth)

    void $ (jsg1 "jQuery" =<< (jsg "document" ! "body")) # "append" $ (outp ! "widget")

    let send e = do
            dat <- toJSData e
            void $ outp # "send" $ dat
    pure (Output send)


makeInterface :: String -> JSM (Input, Output)
makeInterface synthmod = do
    -- instantiate midi tools module, dependency injection style

    tools <- jsgf "MIDITools" (
        jsg "jQuery",
        jsg1 "Keyboard" =<< jsg "jQuery")

    -- XXX hacky DI design
    makesynth <- eval $ "() => new (" ++ synthmod ++ "(Tone)).Synth()"
    

    io <- new (tools ! "IOSelector") (makesynth)

    void $ (jsg1 "jQuery" =<< (jsg "document" ! "body")) # "append" $ (io ! "widget")
    
    poller <- makeMIDIPoller io

    let send e = do
            dat <- toJSData e
            void $ io # "send" $ dat

    pure (Input poller, Output send)

data Event = NoteOn Int Int Int  -- channel (0 based), key, vel normalized with NoteOff = NoteOn 0
    deriving (Show)

fromJSData :: JSVal -> JSM (Maybe Event)
fromJSData dat = do
    d0 <- fromJSValUnchecked =<< dat !! 0
    d1 <- fromJSValUnchecked =<< dat !! 1
    d2 <- fromJSValUnchecked =<< dat !! 2
    let ch = d0 .&. 0x0f
    pure $ case d0 .&. 0xf0 of
        0x80 -> Just $ NoteOn ch d1 0
        0x90 -> Just $ NoteOn ch d1 d2
        _ -> Nothing
    
toJSData :: Event -> JSM JSVal
toJSData (NoteOn ch k v) = toJSVal [0x90 .|. ch, k, v]

makeMIDIPoller :: JSVal -> JSM (JSM (Maybe Event))
makeMIDIPoller interface = do
    queue <- liftIO $ newIORef mempty
    void $ (interface ! "event") # "listen" $ fun \_ _ [e] -> do
        maybee' <- fromJSData e
        forM_ maybee' $ liftIO . modifyIORef queue . Deque.snoc
    pure . liftIO $ atomicModifyIORef queue (\q -> swap (maybe (Nothing, q) (first Just) (Deque.uncons q)))

