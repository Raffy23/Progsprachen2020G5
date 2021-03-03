{- |
Module      :  Lib
Description :  This is the main module for the editor and exposes the main functions for usage
License     :  MIT
-}
module Lib (cursesMain) where

import Prelude hiding (lines)
import Data.Array
import Data.Maybe

import Control.Monad (foldM, (>=>))

import UI.NCurses

import qualified Data.Text as Text

import FileIO
import Curses.UI
import Curses.Types
import Curses.EditorUtils
import Curses.CursesUtils
import Curses.Colors
import Curses.CursesFormatter

import Parser


-- | Entry point of the application, this function takes the first argument as
--   path to a file, contents will be read and displayed in the editor, if no
--   argument is given a new document must be created after F2 is entered ...
cursesMain :: [String] -> IO ()
cursesMain args = runCurses $ do
  {- Setup ncurses environment so that it is usable for us -}
  setEcho False
  setRaw True
  _ <- setCursorMode CursorVisible

  {- Create main window and set it such that we receive all key input events -}
  w <- defaultWindow
  setKeypad w True

  {- Create a sub-window in which we put the text and do the formatting -}
  scrSize@(scrSizeX, scrSizeY) <- screenSize
  a <- newWindow (scrSizeX - 4) (scrSizeY - 2) 3 1
  setKeypad a True

  {- Read file contents, very dirty and use of unsafe, but is sufficient for now ... -}
  fileContents <- return $ convertFileContents scrSize $ getFileContents args

  {- Load colors and draw window border before entering the main loop -}
  editorColors <- initEditorColors
  updateWindow w $ do
    drawDefaultBox
    drawEditorHeader scrSize

  {- Enter the main loop that will only exit when the user presses F12 -}
  cursesLoop $ EditorState w a fileContents NoEvent (0,0) (scrSizeX - 5, scrSizeY - 3) 0 (listToMaybe args) editorColors

  {- Clean-up stuff, close windows and free native resources ... -}
  closeWindow a
  closeWindow w

-- | Main loop of the editor, will poll for events and render the window contents, exits after
--   F12 is pressed
cursesLoop :: EditorState -> Curses ()
cursesLoop state = do
  {- Try to draw content to the window, may fail if window is too small ... -}
  result <- tryCurses $ updateWindow (editor state) $ do
    -- Instead of simple clear command, only clear the last line that can produce
    -- artifacts if only a newline is drawn, should reduce screen flickering ...
    moveCursor (fst $ editorSize state) 0
    clearLine

    -- Reset the cursor and draw the actual content to the screen
    moveCursor 0 0
    drawEditorContents state

    -- Correct the cursor position from the position where we stopped drawing to the actual
    -- cursor position that the user of the editor was
    moveCursor (fst $ cursor state) (min (lineLengthAtCursor state) (snd $ cursor state))

  -- Additional error handling, it will only occur if the cursor is outside the window bounds
  -- If there is an error, only the error message will be displayed in the window
  case result of
    Right _ -> do return ()
    Left ex -> updateWindow (editor state) $ do
      clear
      moveCursor 0 0
      drawString "An error occured:\n"
      drawString (show ex)

  {- Actually render stuff to the native window / flip buffers ... -}
  render

  {- Get cursor position and poll for events, if the exited flag was set then return an empty monad,
     otherwise continue with the loop until one in received -}
  cursor <- getCursor $ editor state
  state' <- processInput $ state { cursor=cursor }
  case state' of
    EditorState { event=Exited      } -> return ()
    EditorState { event=SizeChanged } -> (resizeWindowToTerminal >=> cursesLoop) state'
    _                                 -> cursesLoop state'

-- | Input processing function, does mutate the state of the editor according to the input given,
--   does block until a event was received ...
processInput :: EditorState -> Curses EditorState
processInput state = do
  event <- getEvent (window state) Nothing
  case event of
      Nothing     -> return state
      Just event' -> return $
        case event' of
          {- Special Key handling (F2, F12) -}
          EventSpecialKey (KeyFunction 12) -> state { event=Exited }
          EventSpecialKey (KeyFunction 2)  -> unsafeFileWrite state

          {- Cursor movement -}
          EventSpecialKey KeyUpArrow       -> moveCursorUp state
          EventSpecialKey KeyDownArrow     -> moveCursorDown state
          EventSpecialKey KeyLeftArrow     -> state { cursor=(fst $ cursor state, max 0                        $ (snd $ cursor state) - 1) }
          EventSpecialKey KeyRightArrow    -> state { cursor=(fst $ cursor state, min (snd $ editorSize state) $ (snd $ cursor state) + 1) }

          {- Special editor keys (to move faster around) -}
          EventSpecialKey KeyEnd           -> state { cursor=(fst $ cursor state, min (snd $ editorSize state) $ (lineLengthAtCursor state))}
          EventSpecialKey KeyBegin         -> state { cursor=(fst $ cursor state, 0                                                       ) }
          EventSpecialKey KeyDeleteCharacter -> removeCharacterAfterCursor state

          {- Handle normal text input (including backspace) -}
          EventSpecialKey KeyBackspace     -> removeCharacterAtCursor state
          EventCharacter '\b'              -> removeCharacterAtCursor state
          EventCharacter '\127'            -> removeCharacterAtCursor state
          EventCharacter '\n'              -> newLineAtCursor state
          EventCharacter '\t'              -> appendStringAtCursor state "    "
          EventCharacter char              -> appendCharacterAtCursor state char

          {- Terminal resize event handling (may not work correctly in every terminal) -}
          EventResized                     -> state { event=SizeChanged }

          {- Mouse event handling, modern terminal should be able to position the cursor with the mouse -}
          EventMouse 0 MouseState{mouseCoordinates=(x,y,_)} -> state { cursor=(min (fst $ editorSize state) $ max 0 (y-3), min (snd $ editorSize state) $ min (lineLengthAtCursor state) $ max 0 (x-1)) }

          {- Ignore all other events since we don't really care enough to handle them anyways ... -}
          _ -> state { event=NoEvent }

-- | Convert the file contents from a string into the internal representation
convertFileContents :: (Integer, Integer) -> String -> (Array Integer Text.Text)
convertFileContents (lines, _) input = listArray (0, size) $ text ++ [Text.empty | _ <- [0 :: Integer ..]]
  where
    size = toInteger $ max (length text) (fromIntegral $ lines - 3)
    text = (Text.lines $ Text.pack input)

-- | Draws the contents of the editor state with syntax highlighting and all other features
--   to the screen, text junk after the parsed elements is also drawn in a different color
--   and must not be handled afterwards
drawEditorContents :: EditorState -> Update ()
drawEditorContents state = case parseEditorState state of
  Left ex               -> drawRawText state (show ex)
  Right (result, "")    -> drawWithHighlight result
  Right (result, junk)  -> do
    drawWithHighlight result
    applyAttributesFor [AttributeColor (redText $ colors state)] (drawRawText state junk)

  where
    drawCurrentState astState result = foldM (\s -> draw state s) astState result

    -- TODO: Only virtually traverse tree to find highlight then draw, not double draw at it is now
    drawWithHighlight result = do
      s0 <- drawCurrentState emptyASTState result
      _  <- moveCursor 0 0
      _  <- drawCurrentState (EditorASTState (0,0) (selectedIdentifier s0)) result
      return ()

-- | Resizes the default and the editor window to the new terminal size, should be called after
--   ResizeEvent occurred. The EditorState will be modified appropriately to mirror the resized
--   terminal settings
resizeWindowToTerminal :: EditorState -> Curses EditorState
resizeWindowToTerminal state = do
  scrSize@(scrSizeX, scrSizeY) <- screenSize

  -- Clear window completely and re-draw the borders
  updateWindow (window state) $ do
    clear
    drawDefaultBox
    drawEditorHeader scrSize

  -- Resize virtual editor window to the new size
  updateWindow (editor state) $ do
    clear
    resizeWindow (scrSizeX - 4) (scrSizeY - 2)

  -- Flush changes of window stuff to the screen
  -- before attempting to draw on it again ...
  render

  -- TODO: Maybe use min to position the cursor inside the window after resize ...
  return $ state { event=NoEvent, editorSize=(scrSizeX - 5, scrSizeY - 3), cursor=(0,0) }