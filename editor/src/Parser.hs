{-# LANGUAGE NamedFieldPuns #-}
{- |
Module      :  Parser
Description :  Contains all functions that are related to parsing the given language into an AST so
               that the Editor can do something with it
License     :  MIT
-}
module Parser where

import qualified Data.Text as Text
import Data.Functor.Identity
import Data.Array
import Curses.Types

import Text.Parsec (ParseError, ParsecT)
import Text.Parsec.String (Parser)
import Text.Parsec (parse)
import Text.Parsec.Prim (try)
import Control.Applicative (many, (<|>))
import Text.Parsec.Char (oneOf, char, digit, letter, noneOf)
import Text.Parsec.Combinator (many1, eof, manyTill, anyToken, choice, lookAhead) -- sepBy1, between


-- | A Simple record type that contains all meta information that may be needed when drawing the
--   AST back into a curses window. It is needed since we lose special formatting like whitespaces
--   when converting text into the AST
data CursesEnvironment = CursesEnvironment {
  whitespaces :: Array Int String
}

-- | Sample Language from the PDF
type Env  = CursesEnvironment
data Identifier = Identifier String Bool
data L = Func Env Identifier L
        | BasicL [Basic]
        | ApplyL Env Basic Basic
        | Pair Env Identifier L
        | PairPart Env Char L
        | Junk CursesEnvironment String

data Basic = Number Env Integer
            | Name Env Identifier
            | Expressions Env L
            | Pairs Env [L]
            | JunkBraces Env Char Char String

-- | This function does parse the Editor state into a AST. Either a ParserError if parsing does not
--   work is returned or the AST with all trailing un-parsable junk is returned
parseEditorState :: EditorState -> Either ParseError ([L], String)
parseEditorState EditorState {content, file} =  parseWithLeftOver (many $ choice [try expr, try leftOverText]) flatContent filename
  where
    flatContent = Text.unpack $ Text.unlines $ elems content
    filename    = case file of
                    Just name -> name
                    Nothing   -> ""

{- ---------------------------------------------------------------------------------------------------- -}
{-                                     Language form the PDF                                            -}
{- ---------------------------------------------------------------------------------------------------- -}

expr :: ParsecT String () Identity L
expr = choice [try func, try apply]

func :: ParsecT String () Identity L
func = do
  w1   <- whitespace
  name <- identifier
  w2   <- whitespace
  _    <- char '-'
  _    <- char '>'
  expr <- expr
  return $ Func (CursesEnvironment $ listArray (0,1) [w1, w2]) (Identifier name False) expr

apply :: ParsecT String () Identity L
apply = do
  basic <- many1 basic
  return $ BasicL basic

basic :: ParsecT String () Identity Basic
basic = choice [try basicNumber, try basicName, try basicExpr, try basicPair, try junkBraces]
 where
    junkBraces = do
      w1 <- whitespace
      b  <- choice [char '(', char '{']
      t  <- many $ noneOf [closingBracket b]
      e  <- char $ closingBracket b
      return $  JunkBraces  (CursesEnvironment $ listArray (0,0) [w1]) b e t


basicNumber :: ParsecT String () Identity Basic
basicNumber = do
  w1 <- whitespace
  n  <- number
  return $ Number (CursesEnvironment $ listArray (0,0) [w1]) n

basicName :: ParsecT String () Identity Basic
basicName = do
  w1 <- whitespace
  n  <- identifier
  return $ Name (CursesEnvironment $ listArray (0,0) [w1]) (Identifier n False)

basicExpr :: ParsecT String () Identity Basic
basicExpr = do
  w1   <- whitespace
  _    <- char '('
  expr <- expr
  w2   <- whitespace
  _    <- char ')'
  return $ Expressions (CursesEnvironment $ listArray (0,1) [w1, w2]) expr

basicPair :: ParsecT String () Identity Basic
basicPair = do
  w1   <- whitespace
  _    <- char '{'
  pair <- pairs
  w2   <- whitespace
  _    <- char '}'
  return $ Pairs (CursesEnvironment $ listArray (0,1) [w1, w2]) pair

pair :: ParsecT String () Identity L
pair = do
  w1   <- whitespace
  name <- identifier
  w2   <- whitespace
  _    <- char '='
  expr <- expr
  return $ Pair (CursesEnvironment $ listArray (0,1) [w1, w2]) (Identifier name False) expr

pairs :: ParsecT String () Identity [L]
pairs = do
  pairs <- many f
  return $ pairs
  where
    f = do
      pair <- pair
      w1   <- whitespace
      sep  <- choice [lookAhead $ char '}', char ',']
      return $ PairPart (CursesEnvironment $ listArray (0,0) [w1]) sep pair

junk :: ParsecT String () Identity L
junk = do
  w1   <- whitespace
  junk <- many $ noneOf "({})"
  return $ Junk (CursesEnvironment $ listArray (0,1) [w1]) junk

drainTextUntil :: String -> ParsecT String () Identity L
drainTextUntil exclude = do
  w1   <- whitespace
  junk <- many $ noneOf exclude
  return $ Junk (CursesEnvironment $ listArray (0,1) [w1]) junk

leftOverText :: ParsecT String () Identity L
leftOverText = do
  w1   <- whitespace
  junk <- many1 $ noneOf "(){}\n\r\t "
  return $ Junk (CursesEnvironment $ listArray (0,1) [w1]) junk

parseWithLeftOver :: Parser a -> String -> String -> Either ParseError (a, String)
parseWithLeftOver p c f = parse ((,) <$> p <*> leftOver) f c
  where leftOver = manyTill anyToken eof

whitespace :: Parser String
whitespace = many $ oneOf " \r\n\t"

number :: Parser Integer
number = read <$> many1 digit

identifier :: Parser String
identifier = (:) <$> firstChar <*> many nonFirstChar
  where
    firstChar    = letter <|> char '_'
    nonFirstChar = digit  <|> firstChar

closingBracket :: Char -> Char
closingBracket '(' = ')'
closingBracket '{' = '}'
closingBracket '[' = ']'
closingBracket _ = error "Unknown Bracket!"