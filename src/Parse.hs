{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Parse (test) where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

test :: IO ()
test = print "Parse"

-- Scheme parser

digit :: Parser Text
digit = T.singleton <$> digitChar

letter :: Parser Text
letter = T.singleton <$> letterChar

specialSubsequent :: Parser Text
specialSubsequent = T.singleton <$> oneOf ['+', '-', '.', '@']

peculiarIdentifier :: Parser Text
peculiarIdentifier = T.singleton <$> oneOf ['+', '-']

initial :: Parser Text
initial = letter <|> specialInitial

specialInitial :: Parser Text
specialInitial = T.singleton <$> oneOf ['!', '$', '%', '&', '*', '/', ':', '<', '=', '>', '?', '^', '_', '~']

subsequent :: Parser Text
subsequent = initial <|> digit <|> specialSubsequent

identifier :: Parser Text
identifier = try initial' <|> peculiarIdentifier
  where
    initial' = do
      i <- initial
      s <- many subsequent
      return $ T.concat (i : s)

comment :: Parser Text
comment = do
  _ <- char ';'
  _ <- manyTill anySingle eol
  return ""

type Whitespace = Text

whitespace :: Parser Text
whitespace = T.singleton <$> oneOf [' ', '\n']

atmosphere :: Parser Text
atmosphere = whitespace <|> comment

interTokenSpace :: Parser [Text]
interTokenSpace = many atmosphere


delimiter :: Parser Text
delimiter = choice
  [ T.singleton <$ string " "
  ]

data SyntaticKeyword = Else | RightArrow | Define | Unquote | UnquoteSplicing

syntaticKeyword :: Parser SyntaticKeyword
syntaticKeyword =
  choice
    [ Else <$ string "else",
      RightArrow <$ string "=>",
      Define <$ string "define",
      Unquote <$ string "unquote",
      UnquoteSplicing <$ string "unquote-splicing"
    ]

data ExpressionKeyword
  = Quote
  | Lambda
  | If
  | Set
  | Begin
  | Cond
  | And
  | Or
  | Case
  | Let
  | LetStar
  | LetRec
  | Do
  | Delay
  | Quasiquote

expressionKeyword :: Parser ExpressionKeyword
expressionKeyword =
  choice
    [ Quote <$ string "quote",
      Lambda <$ string "lambda",
      If <$ string "if",
      Set <$ string "set!",
      Begin <$ string "begin",
      Cond <$ string "cond",
      And <$ string "and",
      Or <$ string "or",
      Case <$ string "case",
      Let <$ string "let",
      LetStar <$ string "let*",
      LetRec <$ string "letrec",
      Do <$ string "do",
      Delay <$ string "delay",
      Quasiquote <$ string "quasiquote"
    ]

digit2 :: Parser Text
digit2 = T.singleton <$> oneOf ['0', '1']

digit8 :: Parser Text
digit8 = T.singleton <$> oneOf ['0' .. '7']

digit10 :: Parser Text
digit10 = digit

digit16 :: Parser Char
digit16 = oneOf ['0' .. '9'] <|> oneOf ['a' .. 'f'] <|> oneOf ['A' .. 'F']
