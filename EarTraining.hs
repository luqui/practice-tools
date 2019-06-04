{-# LANGUAGE BlockArguments #-}

import Language.Javascript.JSaddle

main :: JSM ()
main = do
    tools <- jsg1 "JSTools" =<< jsg "jQuery"
    midi <- tools # "MIDIInterface" $ ()
    _ <- (jsg1 "jQuery" =<< (jsg "document" ! "body")) # "append" $ (midi ! "widget")

    _ <- (midi ! "event") # "listen" $ fun \_ _ args -> do
        _ <- jsg "console" # "log" $ args
        pure ()

    pure ()

    
