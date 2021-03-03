{- |
Module      :  Curses.Colors
Description :  Color stuff
License     :  MIT
-}

module Curses.Colors where

import UI.NCurses

import Curses.Types

initEditorColors :: Curses CursesColors
initEditorColors = do
  {- Color Code reference sheet:
     https://upload.wikimedia.org/wikipedia/commons/1/15/Xterm_256color_chart.svg -}

  --                         | Foreground |  Background | ID |
  defaultColor  <- newColorID ColorDefault ColorDefault   1
  blackOnWhite  <- newColorID (Color 15)   ColorBlack     2
  darkYellow    <- newColorID ColorYellow  ColorDefault   3
  lightBlue     <- newColorID (Color 32)   ColorDefault   4
  darkMagenta   <- newColorID (Color 133)  ColorDefault   5
  redBackground <- newColorID ColorDefault (Color 52)     6
  redText       <- newColorID (Color 196)  ColorDefault   7

  return $ CursesColors defaultColor blackOnWhite darkYellow lightBlue darkMagenta redBackground redText