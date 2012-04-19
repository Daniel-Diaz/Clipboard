-- | System clipboard interface with unicode support.
--
-- For more information, see "Graphics.Win32.GDI.Clip"
-- or documentation for /GetClipboardData/ on MSDN.
module System.Clipboard
    ( -- * Clipboard interface
      setClipboardString
    , getClipboardString 
    , modifyClipboardString
      -- * Clipboard format
    , cF_UNICODETEXT
    ) where

import System.Win32.Mem 
    (globalAlloc, globalLock, globalUnlock, copyMemory, gHND)
import Graphics.Win32.GDI.Clip 
    ( openClipboard, closeClipboard, emptyClipboard, 
      getClipboardData, setClipboardData
    , cF_TEXT, ClipboardFormat 
    , isClipboardFormatAvailable)
import Foreign.C
   (withCAString, peekCAString, withCWString, peekCWString)
import Foreign.Ptr
    (castPtr, nullPtr)
import Data.List
    (genericLength)
import Control.Exception 
    (bracket_, bracket)
import Data.Maybe
    (isJust)

-- | Clipboard format of Unicode text.
cF_UNICODETEXT :: ClipboardFormat
cF_UNICODETEXT = 13

-- | Puts a string on the clipboard, using the 'cF_UNICODETEXT' clipboard format.
setClipboardString :: String -> IO ()
setClipboardString str =
   withCWString str $ \cstring -> do
   mem <- globalAlloc gHND strLen
   bracket (globalLock mem) globalUnlock $ \mem' -> do
       copyMemory mem' (castPtr cstring) strLen
       bracket_ (openClipboard nullPtr) closeClipboard $ do
           emptyClipboard
           setClipboardData cF_UNICODETEXT mem'
           return ()
 where
   strLen = 2 * (genericLength str + 1)

-- | Gets the contents of the clipboard as a 'String'.
-- Returns 'Nothing' if the clipboard content is not
-- of type 'cF_TEXT' or 'cF_UNICODETEXT', i.e. it
-- doesn't contain /textual/ data.
getClipboardString :: IO (Maybe String)
getClipboardString =
   bracket_ (openClipboard nullPtr) closeClipboard $ do
     isUnicodeAvailable <- isClipboardFormatAvailable cF_UNICODETEXT
     if isUnicodeAvailable
       then do handle <- getClipboardData cF_UNICODETEXT
               mem <- globalLock handle
               str <- peekCWString (castPtr mem)
               globalUnlock mem
               return $ Just str
       else do isAnsiAvailable <- isClipboardFormatAvailable cF_TEXT
               if isAnsiAvailable
                 then do handle <- getClipboardData cF_TEXT
                         mem <- globalLock handle
                         str <- peekCAString (castPtr mem)
                         globalUnlock mem
                         return $ Just str
                 else return Nothing

-- | Modifies the clipboard content.
-- If the clipboard has /textual/ data, this function modifies its content,
-- and return 'True'. Otherwise, it does nothing and return 'False'.
modifyClipboardString :: (String -> String) -> IO Bool
modifyClipboardString f = do
 s <- getClipboardString
 case s of
   Nothing -> return False
   Just sc -> setClipboardString (f sc) >> return True