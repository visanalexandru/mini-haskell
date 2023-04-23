module Parsing where

import Exp
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
    ( haskellStyle, LanguageDef )
import Text.ParserCombinators.Parsec.Token
import Control.Applicative (some)
import Text.Parsec.Token
import Text.Parsec.String

miniHaskellDef :: LanguageDef st
miniHaskellDef = haskellStyle {reservedNames = (reservedNames haskellStyle) ++ ["let","letrec", "in"], reservedOpNames = (reservedOpNames haskellStyle) ++ ["\\","->", "=", ":="]} 

miniHs :: TokenParser st
miniHs = makeTokenParser miniHaskellDef

testParse :: Parser a -> String -> a
testParse p s
  = case parse p "<input>" s of
      Left err -> error (show err)
      Right a -> a

m_parens = parens miniHs
m_identifier = identifier miniHs
m_reservedOp = reservedOp miniHs
m_reserved = reserved miniHs 
m_operator = operator miniHs
m_semi = semi miniHs
m_semiSep1 = semiSep1 miniHs
m_whiteSpace = whiteSpace miniHs

var :: Parser Var
var = do {x<- (m_identifier <|> (m_operator)); return $ Var x}
-- >>> testParse var "b is a var"
-- Var {getVar = "b"}

varExp :: Parser ComplexExp
varExp = do{ x<- var; return $ CX x } 
-- >>> testParse varExp "b is a var"
-- CX (Var {getVar = "b"})

lambdaExp :: Parser ComplexExp
lambdaExp = do{m_reservedOp ("\\"); variable <-var; m_reservedOp ("->"); expression<-expr; return $ CLam variable expression} 
-- >>> testParse lambdaExp "\\x -> x"
-- CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"}))

letExp :: Parser ComplexExp
letExp = do{m_reserved ("let"); variable <- var; m_reservedOp (":="); expression1 <-expr; m_reservedOp ("in"); expression2 <-expr; return $ Let variable expression1 expression2} 
-- >>> testParse letExp "let x := y in z"
-- Let (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))

letrecExp :: Parser ComplexExp
letrecExp = do{m_reserved ("letrec"); variable <- var; m_reservedOp (":="); expression1 <-expr; m_reservedOp ("in"); expression2 <-expr; return $ LetRec variable expression1 expression2} 
-- >>> testParse letrecExp "letrec x := y in z"
-- LetRec (Var {getVar = "x"}) (CX (Var {getVar = "y"})) (CX (Var {getVar = "z"}))


listExp = do {list <-brackets miniHs (commaSep miniHs varExp); return $ List list}
-- >>> testParse listExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

natExp :: Parser ComplexExp
natExp = do {x<-(natural miniHs); return $ Nat $ fromInteger x} 
-- >>> testParse natExp "223 a"
-- Nat 223

parenExp :: Parser ComplexExp
parenExp = parens miniHs expr 
-- >>> testParse parenExp "(a)"
-- CX (Var {getVar = "a"})

basicExp :: Parser ComplexExp
basicExp = letExp <|> letrecExp  <|> lambdaExp <|> varExp <|> natExp <|> listExp <|> parenExp 
-- >>> testParse basicExp "[a,b,c]"
-- List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})]

expr :: Parser ComplexExp
expr =  do {list <-  (some basicExp); return $ foldl (CApp) (head list) (tail list)}

-- >>> testParse expr "\\x -> [x,y,z]"
-- CLam (Var {getVar = "x"}) (List [CX (Var {getVar = "x"}),CX (Var {getVar = "y"}),CX (Var {getVar = "z"})])

exprParser :: Parser ComplexExp
exprParser = m_whiteSpace *> expr <* eof
-- >>> testParse exprParser "let x := 28 in \\y -> + x y"
-- Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"}))))

