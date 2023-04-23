module Program where
import Exp
import Parsing 
import Printing
import Sugar ( desugarExp, desugarVar )
import Text.Parsec.String (Parser)
import Text.Parsec (many,try , (<|>), eof)
import Eval ( substitute )

import System.IO ( stderr, hPutStrLn )
import qualified Data.Map.Strict as Map

data Definition = Definition
  { defHead :: Var
  , defArgs :: [Var]
  , defBody :: ComplexExp
  }
  deriving (Show)


param :: Parser String
param = (m_identifier  <|> m_operator)

definition :: Parser Definition
definition = do { x<- m_identifier; args <- (many param); y<-m_reserved ":=" ;exp <-expr; return (Definition (Var x) (map Var args) exp)};  

-- >>> parseFirst definition "id := \\x -> x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [], defBody = CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))})

-- >>> parseFirst definition "id x := x"
-- Just (Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})})

-- >>> parseFirst definition "const x y := x"
-- Just (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})



semiSep1' :: Parser a -> Parser [a]
semiSep1' p
 = do
  a <- p
  as <- many $ try (m_semi *> p)
  return (a : as)

program :: Parser [Definition]
program = do { m_whiteSpace; x<- semiSep1' definition; m_semi; return x};

-- >>> parseFirst program "    id x := x ; const x y := x"
-- Nothing

-- >>> parseFirst program "    id x := x ; const x y := x ;"
-- Just [Definition {defHead = Var {getVar = "id"}, defArgs = [Var {getVar = "x"}], defBody = CX (Var {getVar = "x"})},Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})}]

definitionExp :: Definition -> ComplexExp
definitionExp (Definition head [] body ) = body 
definitionExp (Definition head (var : args) body ) = CLam (var) (definitionExp (Definition head args body)) 

-- >>> definitionExp (Definition {defHead = Var {getVar = "const"}, defArgs = [Var {getVar = "x"},Var {getVar = "y"}], defBody = CX (Var {getVar = "x"})})
-- CLam (Var {getVar = "x"}) (CLam (Var {getVar = "y"}) (CX (Var {getVar = "x"})))

type Environment = Map.Map IndexedVar Exp

programEnv :: [Definition] -> Environment
programEnv [] =  Map.empty
programEnv (def : s) = Map.insert (desugarVar $ defHead def) (desugarExp $ definitionExp def)  (programEnv s)

normalizeEnv :: Environment -> Exp -> Exp
normalizeEnv env (X var)= 
 case Map.lookup var env of
  Just exp -> exp
  nothing -> (X var) 

normalizeEnv env (Lam a b) = (Lam a $ normalizeEnv env b) 
normalizeEnv env (App a b) = case left 
 of 
  (Lam x e) -> normalizeEnv env (substitute x b e)
  _ -> App a b
 where 
  left =  normalizeEnv env a
  right = normalizeEnv env b 

