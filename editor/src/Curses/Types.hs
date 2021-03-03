{- |
Module      :  Curses.Types
Description :  All important types of the application are here
License     :  MIT
-}

module Curses.Types where

import Data.Array
import UI.NCurses
import qualified Data.Text as Text

-- | Event type that indicates what kind of event happened in the input loop
data EventType = Exited | SizeChanged | CursorMoved | ContentChanged | NoEvent

-- | A record type for the editor state that holds all kind of information so the
--   application can mutate the state with events
data EditorState = EditorState {
  window     :: !Window,                    -- ^ The reference to the main window of curses
  editor     :: !Window,                    -- ^ The reference to the editor window that contains the text
  content    :: !(Array Integer Text.Text), -- ^ An text line array with all contents of the window
  event      :: !EventType,                 -- ^ Event that lead to the modification of the state
  cursor     :: !(Integer, Integer),        -- ^ The coordinates of the current cursor
  editorSize :: !(Integer, Integer),        -- ^ The size of the editor in the current render state
  lineOffset :: !Integer,                   -- ^ Line offset of how many content lines should be dropped
  file       :: !(Maybe String),            -- ^ File path that is used to save the file
  colors     :: !CursesColors
}

-- | A record type that stores the different Color codes that can be used by the editor
data CursesColors = CursesColors {
  defaultColor  :: !ColorID,
  blackOnWhite  :: !ColorID,
  darkYellow    :: !ColorID,
  lightBlue     :: !ColorID,
  darkMagenta   :: !ColorID,
  redBackground :: !ColorID,
  redText       :: !ColorID
}

-- | A record type that stores the virtual cursor for the AST -> Curses transformation
data EditorASTState = EditorASTState {
  virtualCursor      :: !(Integer, Integer),  -- ^ The current position of the virtual cursor in the
                                              --   content array. It takes the cursor size + offsets
                                              --   into account when being calculated
  selectedIdentifier :: !String               -- ^ The selected identifier which should be highlighted
                                              --   in the sub-tree
}

emptyASTState :: EditorASTState
emptyASTState = EditorASTState (0,0) ""
