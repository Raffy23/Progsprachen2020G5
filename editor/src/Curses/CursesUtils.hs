{-# LANGUAGE NamedFieldPuns #-}
{- |
Module      :  Curses.CursesUtils
Description :  Provides some utilities for curses related formatting
License     :  MIT
-}

module Curses.CursesUtils where

import UI.NCurses hiding (setAttributes)
import Curses.EditorUtils
import Curses.Types
import Curses.UI

-- | Sets all attributes to true in the given list
--   NOTE: This function only exists because the UI.Curses one is bugged and doesn't work ...
setAttributes :: [Attribute] -> Update ()
setAttributes   attributes = mapM_ (\x -> setAttribute x True ) attributes

-- | Unsets all the attributes in the given list
unsetAttributes :: [Attribute] -> Update ()
unsetAttributes attributes = mapM_ (\x -> setAttribute x False) attributes

-- | Used to apply the set of attributes to the next function and then unset everything
applyAttributesFor :: [Attribute] -> Update a -> Update a
applyAttributesFor attributes f = do
  setAttributes attributes
  ret <- f
  unsetAttributes attributes

  return ret

-- | Applies all attributes from the given list to the given Effect when executed
applyAttributesForState :: [Attribute] -> (EditorASTState -> Update a) -> (EditorASTState -> Update a)
applyAttributesForState attributes f = \state -> do
  setAttributes attributes
  ret <- f state
  unsetAttributes attributes

  return ret

-- | Draws the text highlighted if the current virtual cursor is at the position
drawTextWithHighlight :: String -> EditorState -> EditorASTState -> Update EditorASTState
drawTextWithHighlight text state aState@EditorASTState{selectedIdentifier}
  | text == selectedIdentifier = applyAttributesFor attributes $ drawStringIV state aState text
  | otherwise                  = do
    format <- isVirtualCursorAtText state aState text
    drawFormatted format
  where
    attributes          = [AttributeUnderline]
    drawFormatted False = do { drawStringIV state aState text }
    drawFormatted True  = do
      newState <- applyAttributesFor attributes $ drawStringIV state aState text
      return $ newState { selectedIdentifier=text }


-- | A simple parameter swap of the drawStringIV function allowing the use of foldM with partial
--   functions when defining multiple drawXXX functions in a list
drawStringWithState :: String -> EditorState -> EditorASTState -> Update EditorASTState
drawStringWithState s e a = drawStringIV e a s