module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar var = makeIndexedVar $ getVar var

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}

sugarVar :: IndexedVar -> Var
sugarVar indexed = Var $ getName indexed 
 where 
  getName (IndexedVar name 0) = name
  getName (IndexedVar name count) = name ++ "_"++(show count) 

-- >>> sugarVar (IndexedVar "x" 0)
-- Var {getVar = "x"}

-- >>> sugarVar (IndexedVar "x" 3)
-- Var {getVar = "x_3"}

consExp, nilExp, zeroExp, succExp, fixExp :: Exp

consExp = X (makeIndexedVar ":")  -- : :: a -> List a -> List a  list constructor
nilExp = X (makeIndexedVar "Nil") -- Nil :: List a               empty list
zeroExp = X (makeIndexedVar "Z")  -- Z :: Natural                zero
succExp = X (makeIndexedVar "S")  -- S :: Natural -> Natural     successor
fixExp = X (makeIndexedVar "fix") -- fix :: (a -> a) -> a        fixpoint fn.

desugarExp :: ComplexExp -> Exp
desugarExp (CApp a b) = App (desugarExp a) (desugarExp b) 
desugarExp (CLam var e) = Lam (desugarVar var) (desugarExp e) 
desugarExp (CX var) = X $ desugarVar var 
desugarExp (List []) = nilExp 
desugarExp (List (e:expressions)) = App (App (consExp) (desugarExp e)) (desugarExp $ List expressions) 
desugarExp (Nat 0) = zeroExp 
desugarExp (Nat x) = App succExp (desugarExp (Nat $ x-1))
desugarExp (Let var a b) = App (Lam (desugarVar var) (desugarExp b)) (desugarExp a)
desugarExp (LetRec var a b) = App (Lam f e) (App fixExp (Lam f ef))
 where 
  f = desugarVar var
  ef = desugarExp a
  e = desugarExp b 


-- >>> desugarExp (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> desugarExp (Nat 3)
-- App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (X (IndexedVar {ivName = "Z", ivCount = 0}))))

-- >>> desugarExp (List [CX (Var "y"), CX (Var "x")])
-- App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))) (App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "Nil", ivCount = 0})))

-- >>> desugarExp (Let (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0}))

-- >>> desugarExp (LetRec (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (App (X (IndexedVar {ivName = "fix", ivCount = 0})) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))))

sugarExp :: Exp -> ComplexExp
sugarExp (X var) = CX (sugarVar var)  
sugarExp (Lam var e) =  CLam (sugarVar var) (sugarExp e)  
sugarExp (App a b) =  CApp (sugarExp a)  (sugarExp b)

-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))

-- >>> sugarExp (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z"))) 
