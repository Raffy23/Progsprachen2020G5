{-# LANGUAGE NamedFieldPuns #-}
{- |
Module      :  Curses.Utilits
Description :  This module contains various utility functions that are used in the curses program
License     :  MIT
-}

module Curses.EditorUtils where

import Data.Array
import UI.NCurses
import Curses.Types
import qualified Data.Text as Text

-- | Draws a box with the default glyphs in the window
drawDefaultBox :: Update ()
drawDefaultBox = drawBox Nothing Nothing

-- | Moves the cursor into the next line
cursorNewLine :: Update ()
cursorNewLine = do
  position <- cursorPosition
  moveCursor ((fst position) + 1) 0

-- | Returns the current line of the text content (fst cursor + lineOffset)
currentEditorLine :: EditorState -> Integer
currentEditorLine EditorState{lineOffset=lineOffset, cursor=(x, _)} = x + lineOffset

-- | Returns the length of the line the cursor is currently at
lineLengthAtCursor :: EditorState -> Integer
lineLengthAtCursor state @ EditorState { content=content } = fromIntegral $ Text.length $ content ! (currentEditorLine state)

-- | Modifies the state such that the cursor moves up one line in the content, content is also scrolled
--   upwards if necessary. If the cursor is on the top line nothing is changed
moveCursorUp :: EditorState -> EditorState
moveCursorUp state@EditorState{cursor=(0, y), lineOffset} = state { cursor=(0            , y), lineOffset=(max 0 (lineOffset - 1)) }
moveCursorUp state@EditorState{cursor=(x, y)            } = state { cursor=(max 0 $ x - 1, y)                                      }

-- | Modifies the state such that the cursor moves a line down, if the content line is not available
--   a new line is created and the content is extended by one line
moveCursorDown :: EditorState -> EditorState
moveCursorDown state @ EditorState { cursor=(x,y), lineOffset=offset, editorSize=(sX,_), content=content } =
  if (x < sX) then state { cursor=(x + 1, y)                                           }
  else             state { cursor=(sX, y), lineOffset=(offset + 1), content=newContent }
  where
    contentLen = snd $ bounds content
    newContent = listArray (0, contentLen + 1) $ (elems content) ++ [Text.empty]

-- | Removes a character at the cursor position from the content, if the the cursor is at the
--   beginning of a line it is moved to the end of the above line
removeCharacterAtCursor :: EditorState -> EditorState
removeCharacterAtCursor state @ EditorState{ content=content, cursor=(x,y), lineOffset } =
  if empty then state {
    cursor     = (max 0 $ x - 1, fromIntegral (Text.length $ content ! (max 0 $ cLineIndex - 1))),
    lineOffset = if x-1 < 0 then max 0 (lineOffset-1) else lineOffset
  } else if y == 0 && x > 0 then state {
    content    = mergedLines,
    cursor     = (x - 1, mergedLinesLen)
  } else state {
    content    = removeFromText,
    cursor     = (x, max 0 $ y - 1)
  }
  where
    cLineIndex     = currentEditorLine state
    empty          = Text.null $ content ! cLineIndex
    removeFromText = content // [(cLineIndex, Text.append firstPart (snd split))]
    split          = Text.splitAt (fromIntegral y) $ content ! cLineIndex
    firstPart      = Text.dropEnd 1 (fst split)
    mergedLines    = content // [(cLineIndex - 1, Text.append (content ! (cLineIndex-1)) (content ! cLineIndex)), (cLineIndex, Text.empty)]
    mergedLinesLen = fromIntegral $ Text.length $ mergedLines ! (cLineIndex-1)

-- | Removes a character after the cursor position, does nothing if the current line is empty
removeCharacterAfterCursor :: EditorState -> EditorState
removeCharacterAfterCursor state @ EditorState{ content=content, cursor=(x,y) } =
 if empty then state else state {
   content=removeFromText,
   cursor=(x, max 0 $ y - 1)
 }
 where
   cLineIndex     = currentEditorLine state
   empty          = Text.null $ content ! cLineIndex
   removeFromText = content // [(cLineIndex, Text.append (fst split) (sndPart))]
   split          = Text.splitAt (fromIntegral y) $ content ! cLineIndex
   sndPart        = Text.drop 1 (snd split)

-- | Appends a character at the current position to the text, new line should not be passed to
--   this function since it will be rendered and the content positions don't match the screen ones
appendCharacterAtCursor :: EditorState -> Char -> EditorState
appendCharacterAtCursor state @ EditorState { content=content, cursor=(x,y) } char =
  state {
      content=insertInText,
      cursor=(x, y + 1)
  }
  where
    cLineIndex      = currentEditorLine state
    contentTextHead = Text.take (fromIntegral y) (content ! cLineIndex)
    contentTextTail = Text.drop (fromIntegral y) (content ! cLineIndex)
    newContentText  = Text.append (Text.snoc contentTextHead char) contentTextTail
    insertInText = content // [(cLineIndex, newContentText)]

-- | Similar to 'appendCharacterAtCursor' but appends a whole string to the content instead of a
--   single char
appendStringAtCursor :: EditorState -> String -> EditorState
appendStringAtCursor state @ EditorState { content=content, cursor=(x,y) } str =
  state {
      content=insertInText,
      cursor=(x, y + strLength)
  }
  where
    strLength       = fromIntegral $ length str
    cLineIndex      = currentEditorLine state
    contentTextHead = Text.take (fromIntegral y) (content ! cLineIndex)
    contentTextTail = Text.drop (fromIntegral y) (content ! cLineIndex)
    contentNewHead  = Text.append contentTextHead $ Text.pack str
    newContentText  = Text.append contentNewHead contentTextTail
    insertInText = content // [(cLineIndex, newContentText)]

-- | Inserts a new line at the current cursor position and moves the cursor to the end of the new
--   line. This function should be called if a '\n' should be inserted into the text
newLineAtCursor :: EditorState -> EditorState
newLineAtCursor state @ EditorState{ content, cursor=(_,y) } =
  moveCursorDown $ state { content=newContent }
  where
    cLineIndex      = currentEditorLine state
    currentLineIdx  = fromIntegral cLineIndex
    contentLen      = snd $ bounds content
    contentElems    = elems content
    contentTextHead = Text.take (fromIntegral y) (content ! cLineIndex)
    contentTextTail = Text.drop (fromIntegral y) (content ! cLineIndex)
    beforeLines     = take currentLineIdx contentElems
    afterLines      = drop (currentLineIdx+1) contentElems
    newContent      = listArray (0, contentLen + 1) $ beforeLines ++ [contentTextHead, contentTextTail] ++ afterLines

-- | Checks if the current cursor position in the curses window + offset is at the position where
--   the AST string would be drawn
isVirtualCursorAtText :: EditorState -> EditorASTState -> String -> Update Bool
isVirtualCursorAtText EditorState {cursor=(dCurX,curY),lineOffset=offset} EditorASTState{virtualCursor=(curX,_)} text = do
  (_, dCurY) <- cursorPosition
  return $ curX == (dCurX + offset) && curY >= dCurY && curY < (dCurY + (fromIntegral $ length text))




