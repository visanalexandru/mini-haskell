module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar v = getVar v

showExp :: ComplexExp -> String
showExp (CX var) = showVar var 
showExp (Nat x) = show x
showExp (CLam v e) = "\\" ++ (showVar v) ++ " -> " ++ (showExp e)
showExp (CApp e1 e2) = showExp e1 ++ " " ++ showExp e2 
showExp (Let v e1 e2) = "let " ++ (showVar v) ++ " := " ++ (showExp e1)  ++ " in " ++ (showExp e2)
showExp (LetRec v e1 e2) = "letrec " ++ (showVar v) ++ " := " ++ (showExp e1)  ++ " in " ++ (showExp e2)
showExp (List l) = show l 

instance Show ComplexExp where
 show = showExp
