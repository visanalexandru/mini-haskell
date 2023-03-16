module REPLCommand where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef, LanguageDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec 

data REPLCommand
  = Quit
  | Load String
  | Eval String
 deriving Show

parseQuit :: Parser REPLCommand
parseQuit = do{(string "q");skipMany space;eof; return Quit}

parseLoad :: Parser REPLCommand
parseLoad = do {((string "l"));skipMany1 space; file <- (many1 letter); skipMany space; eof;return $ Load file}

parseCommand = do{char ':'; parseQuit <|> parseLoad} 

parseEval :: Parser REPLCommand
parseEval = do {command <- many1 anyChar; eof; return $ Eval command} 

replCommand :: Parser REPLCommand
replCommand =  parseCommand <|> parseEval

