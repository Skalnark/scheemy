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

data Program = SToken 
             | SIdentifier 
             | SBoolean 
             | SCharacter 
             | SString 
             | SNumber 
             | SDelimiter 
             | SintaticKeyword 
             | ExpressionKeyword
  deriving (Show)

program :: Parser Program
program = do
  _ <- optional interTokenSpace
  choice 
    [ SToken <$ pToken,
      SDelimiter <$ delimiter,
      SintaticKeyword <$ syntaticKeyword,
      ExpressionKeyword <$ expressionKeyword
    ]

test :: Text -> IO ()
test = parseTest (program <* eof)

-- Scheme syntax

pToken :: Parser Text
pToken = identifier <|> boolean <|> character <|> stringElement <|> number <|> delimiter

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
  _ <- manyTill anySingle newline
  return ""

whitespace :: Parser Text
whitespace = do
  _ <- oneOf [' ', '\t', '\n']
  return ""

atmosphere :: Parser Text
atmosphere = whitespace <|> comment

interTokenSpace :: Parser [Text]
interTokenSpace = many atmosphere


delimiter :: Parser Text
delimiter = choice
  [ T.singleton <$> oneOf [' ', '(', ')', '"', ';']
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


variable :: Parser Text
variable = identifier

boolean :: Parser Text
boolean = choice [string "#t", string "#f"]

character :: Parser Text
character = do
  _ <- string "#\\"
  choice
    [ T.singleton <$> anySingle,
      namedCharacter
    ]
  where
    namedCharacter = choice
      [ "space" <$ string "space",
        "newline" <$ string "newline"
      ]

stringElement :: Parser Text
stringElement = do
  _ <- oneOf ['\\', '"']
  choice
    [ T.singleton <$> anySingle,
      escapeSequence
    ]
  where
    escapeSequence = do
      _ <- char '\\'
      choice
        [ string "\\",
          string "\""
        ]

number :: Parser Text
number = choice
  [ decimal,
    octal,
    hexadecimal,
    binary
  ]
  where
    decimal = digit10
    octal = digit8
    hexadecimal = digit16
    binary = digit2

digit2 :: Parser Text
digit2 = T.singleton <$> oneOf ['0', '1']

digit8 :: Parser Text
digit8 = T.singleton <$> oneOf ['0' .. '7']

digit10 :: Parser Text
digit10 = digit

digit16 :: Parser Text
digit16 = T.singleton <$> oneOf (['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F'])
