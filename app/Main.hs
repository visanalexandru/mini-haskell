module Main where

import System.IO
import System.Console.Isocline
import Text.Parsec (parse)
import Text.Parsec.String (parseFromFile)

import Program
import Exp
import Parsing
import Printing
import REPLCommand
import Eval
import Sugar 
import qualified Data.Map.Strict as Map


transform :: Environment -> ComplexExp -> ComplexExp 
transform env expression = sugarExp $ normalizeEnv env simple 
 where simple = desugarExp expression

processRepl :: Environment -> REPLCommand -> IO()
processRepl env (Quit) =  do {return ()} 

processRepl env (Load filename ) = 
 do 
  result <- parseFromFile program filename
  case result of
   Left error -> do {putStrLn $ show error; execute env}
   Right definitions -> do {putStrLn $ "Loaded " ++ filename; putStrLn $ show $ programEnv definitions ; execute (programEnv definitions)}

processRepl env (Eval string) = 
 case parse exprParser "command" string of
  Left err -> do { putStrLn $ show err; execute env}
  Right a -> do {putStrLn $ show $ transform env a; execute env}

processCommand :: Environment -> String -> IO ()
processCommand env x = 
 case parse replCommand "input" x of 
  Left err -> do{ putStrLn "\ESC[31m";putStrLn $ show err;putStrLn $ "\ESC[0m"; execute env}
  Right a -> processRepl env a 

execute :: Environment -> IO ()
execute env = 
 do 
  putStr "\ESC[92m> \ESC[0m"
  x<- getLine 
  processCommand env x


toInt x = case x of 
 (Just v) -> v
 nothing -> 0

main :: IO ()
main =  execute Map.empty
  

