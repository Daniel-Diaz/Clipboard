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
import System.IO (hClose, stdin, stdout, stderr)
import Foreign.C.Types (CChar, CUChar)
import Foreign.Marshal.Array (withArrayLen)
import Codec.Binary.UTF8.String (decode, encode)

getClipboardString :: IO (Maybe String)
getClipboardString = do
    (display, window, clipboard) <- initialSetup
    inp <- internAtom display "clipboard_get" False
    target <- internAtom display "UTF8_STRING" True
    xConvertSelection display clipboard target inp window currentTime
    ret <- clipboardInputWait display window inp
    cleanup display window
    return ret

clipboardInputWait :: Display -> Window -> Atom -> IO (Maybe String)
clipboardInputWait display window inp = do
    ev <- getNextEvent display
    if ev_event_type ev == selectionNotify
        then (charsToString <$>) <$> getWindowProperty8 display inp window
        else clipboardInputWait display window inp

charsToString :: [CChar] -> String
charsToString = decode . map fromIntegral


setClipboardString :: String -> IO ()
setClipboardString str = do
    (display, window, clipboard) <- initialSetup
    xSetSelectionOwner display clipboard window currentTime
    void $ forkProcess $ do
        hClose stdin
        hClose stdout
        hClose stderr
        clipboardOutputWait display $ stringToChars str
        cleanup display window

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


initialSetup :: IO (Display, Window, Atom)
initialSetup = do
    display <- openDisplay ""
    window <- createSimpleWindow display (defaultRootWindow display)
                                 0 0 1 1 0 0 0
    clipboard <- internAtom display "CLIPBOARD" True
    return (display, window, clipboard)

cleanup :: Display -> Window -> IO ()
cleanup display window = do
    destroyWindow display window
    closeDisplay display

getNextEvent :: Display -> IO Event
getNextEvent display = allocaXEvent $ \ev -> do
    nextEvent display ev
    getEvent ev
