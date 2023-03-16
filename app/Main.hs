module Main where

import System.IO
import System.Console.Isocline
import Text.Parsec (parse)

import Exp
import Parsing
import Printing
import REPLCommand

processRepl :: REPLCommand -> IO()
processRepl (Quit) =  do {return ()} 
processRepl (Load filename ) = do{putStrLn $ "load file " ++ filename ; main}
processRepl (Eval string) = 
 case parse exprParser "command" string of
  Left err -> do {putStrLn $ show err; main}
  Right a -> do {putStrLn $ show a; main}

processCommand :: String -> IO ()
processCommand x = 
 case parse replCommand "input" x of 
  Left err -> do{putStrLn $ show err; main}
  Right a -> processRepl a 

main :: IO ()
main = 
 do
  putStr "> "
  x<- getLine 
  processCommand x

