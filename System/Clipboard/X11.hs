module System.Clipboard.X11
( getClipboardString
, setClipboardString
) where

import Control.Monad
import Control.Concurrent
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib
import Data.Functor
import Codec.Binary.UTF8.String

setClipboardString = undefined

getClipboardString :: IO (Maybe String)
getClipboardString = withInitialSetup $ \display window -> do
    clipboard <- internAtom display "CLIPBOARD" True
    inp <- internAtom display "clipboard_get" False
    target <- internAtom display "UTF8_STRING" True
    xConvertSelection display clipboard target inp window currentTime
    clipboardInputWait display window inp

clipboardInputWait :: Display -> Window -> Atom -> IO (Maybe String)
clipboardInputWait display window inp = do
    ev <- getNextEvent display
    if ev_event_type ev == selectionNotify
        then (charsToString <$>) <$> getWindowProperty8 display inp window
        else clipboardInputWait display window inp

charsToString = decode . map fromIntegral

withInitialSetup :: (Display -> Window -> IO a) -> IO a
withInitialSetup f = do
    display <- openDisplay ""
    window <- createSimpleWindow display (defaultRootWindow display) 0 0 1 1 0 0 0
    ret <- f display window
    destroyWindow display window
    closeDisplay display
    return ret

getNextEvent display = allocaXEvent $ \ev -> do
    nextEvent display ev
    getEvent ev
