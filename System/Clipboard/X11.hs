module System.Clipboard.X11
( getClipboardString
, setClipboardString
) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Posix.Process (forkProcess)

import Data.Maybe
import Data.Functor
import Control.Monad
import Foreign.C.Types (CChar, CUChar)
import Foreign.Marshal.Array (withArrayLen)
import Codec.Binary.UTF8.String (decode, encode)

getClipboardString :: IO (Maybe String)
getClipboardString = withInitialSetup $ \display window clipboard -> do
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

charsToString :: [CChar] -> String
charsToString = decode . map fromIntegral


setClipboardString :: String -> IO ()
setClipboardString str = void $ forkProcess $
    withInitialSetup $ \display window clipboard -> do
        xSetSelectionOwner display clipboard window currentTime
        clipboardOutputWait display $ stringToChars str

clipboardOutputWait :: Display -> [CUChar] -> IO ()
clipboardOutputWait display str = do
    let loop = clipboardOutputWait display str
    ev <- getNextEvent display
    case ev of
        SelectionRequest { ev_requestor = req
                         , ev_target = target
                         , ev_property = prop
                         , ev_selection = sel
                         , ev_time = time} -> do
            target' <- getAtomName display target
            res <- handleOutput display req prop target' str
            sendSelectionNotify display req sel target res time
            loop
        _ -> unless (ev_event_type ev == selectionClear) loop

handleOutput :: Display -> Window -> Atom -> Maybe String -> [CUChar] -> IO Atom
handleOutput display req prop (Just "UTF8_STRING") str = do
    prop' <- getAtomName display prop
    if isNothing prop' then handleOutput display req prop Nothing str else do
        target <- internAtom display "UTF8_STRING" True
        void $ withArrayLen str $ \len str' ->
            xChangeProperty display req prop target 8 propModeReplace str'
                            (fromIntegral len)
        return prop
handleOutput _ _ _ _ _ = return none

sendSelectionNotify :: Display -> Window -> Atom -> Atom -> Atom -> Time ->
                           IO ()
sendSelectionNotify display req sel target prop time = allocaXEvent $ \ev -> do
    setEventType ev selectionNotify
    setSelectionNotify ev req sel target prop time
    sendEvent display req False 0 ev

stringToChars :: String -> [CUChar]
stringToChars = map fromIntegral . encode


withInitialSetup :: (Display -> Window -> Atom -> IO a) -> IO a
withInitialSetup f = do
    display <- openDisplay ""
    window <- createSimpleWindow display (defaultRootWindow display)
                                 0 0 1 1 0 0 0
    clipboard <- internAtom display "CLIPBOARD" True
    ret <- f display window clipboard
    destroyWindow display window
    closeDisplay display
    return ret

getNextEvent :: Display -> IO Event
getNextEvent display = allocaXEvent $ \ev -> do
    nextEvent display ev
    getEvent ev
