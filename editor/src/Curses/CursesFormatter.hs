{-# LANGUAGE NamedFieldPuns #-}
{- |
Module      :  Curses.CursesFormatter
Description :  provides the implementation for the draw function of all CursesFormatter instances
License     :  MIT
-}

module Curses.CursesFormatter where

import Data.Array
import Data.List (intersperse)
import Data.List.Split (splitWhen)
import Control.Monad (foldM)
import UI.NCurses hiding (setAttributes)
import Curses.Types
import Curses.UI
import Curses.CursesUtils
import Curses.EditorUtils
import Parser

-- | CursesFormatter class defines all functions that can be used to draw a AST into the curses window
--   It is defined in a generic way to be able to support multiple independent AST implementations
class CursesFormatter a where
  -- Draws the contents of the given AST Element with the EditorState and the EditorASTState to the
  -- window, the function must check if drawing at the current cursor position is possible
  draw :: EditorState -> EditorASTState -> a -> Update EditorASTState
  draw' :: a -> EditorState -> EditorASTState -> Update EditorASTState
  draw' a state s0 = draw state s0 a

instance CursesFormatter L where
  draw state s0 (Func CursesEnvironment{whitespaces=w} name expr) = do
    drawAll state s0 [
        drawWhitespaceWithState (w ! 0)
      , drawIdentifier name
      , drawWhitespaceWithState (w ! 1)
      , drawStringWithState "->"
      , draw' expr
      ]

  draw state s0 (BasicL basic) = drawAllCF state s0 basic

  draw state s0 (ApplyL CursesEnvironment{whitespaces=w} apply basic) = do
    drawAll state s0 [
        drawWhitespaceWithState (w ! 0)
      , draw' apply
      , drawWhitespaceWithState (w ! 1)
      , draw' basic
      ]

  draw state s0 (Pair CursesEnvironment{whitespaces=w} name expr) = do
    drawAll state s0 [
        drawWhitespaceWithState (w ! 0)
      , drawIdentifier name
      , drawWhitespaceWithState (w ! 1)
      , drawStringWithState "="
      , draw' expr
      ]

  draw state s0 (PairPart CursesEnvironment{whitespaces=w} sep pair) = do
    drawAll state s0 [
        draw' pair
      , drawWhitespaceWithState (w ! 0)
      , drawStringWithState (if sep == ',' then "," else "")
      ]


  draw state@EditorState{colors=CursesColors{redBackground}} s0 (Junk CursesEnvironment{whitespaces=w} junk) = do
    applyAttributesFor [AttributeColor redBackground] $ drawAll state s0 [
        drawWhitespaceWithState (w ! 0)
      , drawStringWithState junk
      ]

instance CursesFormatter Basic where
  draw state s0 (Expressions CursesEnvironment{whitespaces=w} l) = do
    s1 <- drawWhitespaceWithState (w ! 0) state s0
    drawBraces state s1 $ drawAll' [
        draw' l
      , drawWhitespaceWithState (w ! 1)
      ]

  draw state s0 (Pairs CursesEnvironment{whitespaces=w} l) = do
    s1 <- drawStringIV state s0 (w ! 0)
    drawCurlyBraces state s1 $ drawAll' [
        drawAllCF' l
      , drawWhitespaceWithState (w ! 1)
      ]

  draw state s0 (Number CursesEnvironment{whitespaces=w} number) = do
    drawAll state s0 [
        drawWhitespaceWithState (w ! 0)
      , drawNumber (show number)
      ]

  draw state s0 (Name CursesEnvironment{whitespaces=w} name) = do
    drawAll state s0 [
        drawWhitespaceWithState (w ! 0)
      , drawIdentifier name
      ]

  draw state@EditorState{colors=CursesColors{redBackground}} s0 (JunkBraces CursesEnvironment{whitespaces=w} b e junk) = do
    applyAttributesFor [AttributeColor redBackground] $ drawAll state s0 [
        drawStringWithState (w ! 0)
      , drawMatchingSymbols' [AttributeReverse] [b] [e] (drawStringWithState junk)
      ]


-- | Draws the braces ( and ) around a entry, if the cursor is at the position of the braces these
--   get highlighted
drawBraces :: EditorState -> EditorASTState -> (EditorState -> EditorASTState -> Update EditorASTState) -> Update EditorASTState
drawBraces = drawMatchingSymbols [AttributeReverse] "(" ")"

drawCurlyBraces :: EditorState -> EditorASTState -> (EditorState -> EditorASTState -> Update EditorASTState) -> Update EditorASTState
drawCurlyBraces = drawMatchingSymbols [AttributeReverse] "{" "}"

-- | Draws all elements in the list to the current Editor state
drawAll :: EditorState -> EditorASTState -> [(EditorState -> EditorASTState -> Update EditorASTState)] -> Update EditorASTState
drawAll state = foldM (\s f -> f state s)

drawAllCF :: CursesFormatter a => EditorState -> EditorASTState -> [a] -> Update EditorASTState
drawAllCF state = foldM (\s cf -> draw state s cf)

-- | Same as drawAll but with flipped arguments such that it is possible to be used with drawBraces
drawAll' :: [(EditorState -> EditorASTState -> Update EditorASTState)] -> EditorState -> EditorASTState -> Update EditorASTState
drawAll' list state s0 = foldM (\s f -> f state s) s0 list

drawAllCF' :: CursesFormatter a => [a] -> EditorState -> EditorASTState -> Update EditorASTState
drawAllCF' list state s0 = foldM (\s cf -> draw state s cf) s0 list

-- | Draws the symbols start and end with the given attributes of one of them is hovered by the
--   cursor in the window
drawMatchingSymbols :: [Attribute] -> String -> String -> EditorState -> EditorASTState -> (EditorState -> EditorASTState -> Update EditorASTState) -> Update EditorASTState
drawMatchingSymbols attrs startSym endSym state s0 f = do
  -- Save the current cursor state, we need that for backtracking
  (curSymX, curSymY)   <- cursorPosition
  hoverStartingSymbol  <- isVirtualCursorAtText state s0 startSym

  -- Draw the first symbol and the other stuff
  s1 <- drawStringWithState startSym state s0
  s2 <- f state s1

  -- Now the complex part begins where we may need to backtrack and correct the previously drawn symbol
  hoverEndingBracket  <- isVirtualCursorAtText state s2 endSym
  s3 <- drawEndSym hoverStartingSymbol s2

  backtrackStartSym hoverEndingBracket curSymX curSymY s3
  where
    drawEndSym :: Bool -> EditorASTState -> Update EditorASTState
    drawEndSym True  s = applyAttributesFor attrs $ drawStringWithState endSym state s
    drawEndSym False s = drawStringWithState endSym state s

    backtrackStartSym :: Bool -> Integer -> Integer -> EditorASTState -> Update EditorASTState
    backtrackStartSym False _ _ s                       = return s
    backtrackStartSym True curStartSymX curStartSymY s0 = do
      -- Save cursor position and restore after draw ...
      (curEndSymX, curEndSymY) <- cursorPosition
      moveCursor curStartSymX curStartSymY

      -- re-draw start symbol with the applied attributes
      _ <- applyAttributesFor attrs $ drawStringWithState startSym state s0

      -- reset cursor and supply the current state
      moveCursor curEndSymX curEndSymY
      return s0

drawMatchingSymbols' :: [Attribute] -> String -> String -> (EditorState -> EditorASTState -> Update EditorASTState) -> EditorState -> EditorASTState -> Update EditorASTState
drawMatchingSymbols' attr open close f state s0 = drawMatchingSymbols attr open close state s0 f

-- | Draws a String with the formatting of a identifier entry to the editor state
drawIdentifier :: Identifier -> EditorState -> EditorASTState -> Update EditorASTState
drawIdentifier (Identifier name _) state@EditorState{colors=CursesColors{darkYellow}} s0 = do
  applyAttributesFor [AttributeColor darkYellow] $ drawTextWithHighlight name state s0

-- | Draws a String with the formatting of a number entry to the editor state
drawNumber :: String -> EditorState -> EditorASTState -> Update EditorASTState
drawNumber value state@EditorState{colors=CursesColors{lightBlue}} s0 = do
  applyAttributesFor [AttributeColor lightBlue] $ drawStringWithState value state s0

-- | This function draws whitespace characters by splitting them up by newlines so that
--   text culling works fine in the editor space
drawWhitespaceWithState :: String -> EditorState -> EditorASTState -> Update EditorASTState
drawWhitespaceWithState s e a = drawAll' stringContent e a
  where
    splitContent  = splitWhen (=='\n') s
    withNewLines  = intersperse "\n" splitContent
    stringContent = map drawStringWithState withNewLines
