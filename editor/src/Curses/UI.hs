{-# LANGUAGE NamedFieldPuns #-}
{- |
Module      :  Curses.UI
Description :  This module contains all functions that modify the curses window state
License     :  MIT
-}
module Curses.UI where

import Data.List.Split
import Data.Array
import UI.NCurses
import Curses.Types

-- | Draws all menu elements and the header of the editor, first parameter should be the size of the
--   curses window
drawEditorHeader :: (Integer, Integer) -> Update ()
drawEditorHeader (_, width) = do
  moveCursor 1 2
  drawString "Synatax aware editor  "
  drawGlyph glyphLineV
  drawMenuElement "F2" "Save"
  drawMenuElement "F12" "Exit"
  moveCursor 2 0
  drawGlyph glyphTeeL
  moveCursor 2 1
  drawLineH Nothing (width-2)
  moveCursor 2 (width-1)
  drawGlyph glyphTeeR

-- | Draws a menu element with simple text formatting
drawMenuElement :: String -> String -> Update ()
drawMenuElement header string = do
  drawString "  "
  setAttribute AttributeBold True
  drawString header
  setAttribute AttributeBold False
  drawString ":"
  drawString string

-- | Draws the raw string from the EditorState to the editor window, no code styling is done
--   This function will also only draw as much text as it can fit into the editor, text will be
--   truncated / taken at the offset depending on the given EditorState
drawEditorText :: EditorState -> Update ()
drawEditorText EditorState { content, editorSize=(sX, _), lineOffset=offset } = do
  mapM_ draw $ zip [0..] visibleLines
  where
    allLines     = elems content
    visibleLines = take ((fromIntegral sX) + 1) $ drop (fromIntegral offset) allLines
    draw (i, l)
      | i == sX   = do { drawText l }
      | otherwise = do { drawText l; drawString "\n" }

-- | Draws the given raw text to the editor window, no code styling is done
--   This function will also only draw as much text as it can fit into the editor,
--   text will be truncated / taken at the offset depending on the given EditorState
drawRawText :: EditorState -> String -> Update ()
drawRawText EditorState { editorSize=(sX, _) } content = do
  (cX, _) <- cursorPosition
  mapM_ draw $ zip [cX..] (visibleLines cX)
  where
    allLines        = splitOn "\n" content
    visibleLines cX = take (fromIntegral (sX-cX) + 1) allLines
    draw (i, l)
      | i == sX   = do { drawString l }
      | otherwise = do { drawString l; drawString "\n" }

-- | Draws a given string according to the EditorState and EditorASTState parameters into the window,
--   This function will only draw as much text as it can fit into the editor, text will be
--   truncated / taken at the offset depending on the given EditorState and EditorASTState
--   A new EditorASTState is the result of the function moving the virtual cursor to the new position
drawStringIV :: EditorState -> EditorASTState -> String -> Update EditorASTState
drawStringIV EditorState{ lineOffset=offset, editorSize=(eX, _) } state@EditorASTState{ virtualCursor=(vX, vY) } text
  | isInView    = do { drawString text; return newState }
  | otherwise   = return newState
  where
    isInView  = vX >= offset && (vX + textSizeX) <= (eX + offset)
    textSize  = foldl (\(x,y) c -> if c == '\n' then (x+1, 0) else (x, y+1)) (0,0) text
    textSizeX = fst textSize
    textSizeY = snd textSize
    newState
      | textSizeX > 0 = state { virtualCursor=(vX + textSizeX, textSizeY     ) }
      | otherwise     = state { virtualCursor=(vX            , vY + textSizeY) }