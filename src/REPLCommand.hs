module REPLCommand where

import Data.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, LanguageDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec 

data REPLCommand
  = Quit
  | Load String
  | Eval String
 deriving Show


-- Check if a character is not whitespace.
notWhitespace :: Char -> Bool
notWhitespace c = not $ isSpace c 

-- Parses everything except whitespace.
nonWhitespaceParser :: Parser String
nonWhitespaceParser = many1 $ satisfy  notWhitespace

parseQuit :: Parser REPLCommand
parseQuit = do{(try (string "quit")) <|> (string "q");skipMany space;eof; return Quit}

parseLoad :: Parser REPLCommand
parseLoad = do {(try (string "load")) <|> (string "l");skipMany1 space; file <- (nonWhitespaceParser); skipMany space; eof;return $ Load file}

parseCommand = do{char ':'; parseQuit <|> parseLoad} 

parseEval :: Parser REPLCommand
parseEval = do {command <- many anyChar; eof; return $ Eval command} 

replCommand :: Parser REPLCommand
replCommand =  parseCommand <|> parseEval

