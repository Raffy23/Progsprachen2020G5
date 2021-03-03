{- |
Module      :  FileIO
Description :  Helper functions for file IO are in this module
License     :  MIT
-}
module FileIO where

import Data.Array
import Control.Exception
import System.IO.Unsafe

import qualified Data.Text as Text

import Curses.Types

-- | Performs an unsafe IO operations and reads the file contents from the first element of the given
--   array, returns an empty string if the file can not be read or if the array is empty
getFileContents :: [String] -> String
getFileContents [filePath]    = unsafePerformIO $ readFile filePath `catch` \e -> const (return "")(e :: IOException)
getFileContents _             = ""

-- | Performs an unsafe IO operations and writes the file contents to the disk, the editor
--   state must have set a file otherwise this function will do nothing
unsafeFileWrite :: EditorState -> EditorState
unsafeFileWrite state @ EditorState { file=(Just file), content=content } = unsafePerformIO $ do
  writeFile file $ Text.unpack $ Text.unlines (elems content)
  return state
unsafeFileWrite state = state