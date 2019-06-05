{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments #-}

import Prelude hiding ((!!))
import Control.Arrow (first)
import Control.Concurrent (forkIO)
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.))
import Data.IORef
import Data.Tuple (swap)
import qualified Deque.Strict as Deque
import Language.Javascript.JSaddle

main :: JSM ()
main = do
    tools <- jsg1 "JSTools" =<< jsg "jQuery"
    midi <- tools # "MIDIInterface" $ ()
    void $ (jsg1 "jQuery" =<< (jsg "document" ! "body")) # "append" $ (midi ! "widget")
    
    poller <- makeMIDIPoller midi

    void . liftIO . forkIO . forever $ do
        void waitForAnimationFrame
        maybee <- poller 
        forM_ maybee $ \e -> do
            doc <- jsg "document"
            _ <- doc # "write" $ toJSVal (toJSString (show e))
            pure ()

    pure ()

data MIDIEvent = NoteOn Int Int Int  -- channel, key, vel normalized with NoteOff = NoteOn 0
    deriving (Show)

convertEvent :: JSVal -> JSM (Maybe MIDIEvent)
convertEvent v = do
    dat <- v ! "data"
    d0 <- fromJSValUnchecked =<< dat !! 0
    d1 <- fromJSValUnchecked =<< dat !! 1
    d2 <- fromJSValUnchecked =<< dat !! 2
    let ch = d0 .&. 0x0f
    pure $ case d0 .&. 0xf0 of
        0x80 -> Just $ NoteOn ch d1 0
        0x90 -> Just $ NoteOn ch d1 d2
        _ -> Nothing
    

makeMIDIPoller :: JSVal -> JSM (JSM (Maybe MIDIEvent))
makeMIDIPoller interface = do
    queue <- liftIO $ newIORef mempty
    void $ (interface ! "event") # "listen" $ fun \_ _ [e] -> do
        maybee' <- convertEvent e
        forM_ maybee' $ liftIO . modifyIORef queue . Deque.snoc
    pure . liftIO $ atomicModifyIORef queue (\q -> swap (maybe (Nothing, q) (first Just) (Deque.uncons q)))

