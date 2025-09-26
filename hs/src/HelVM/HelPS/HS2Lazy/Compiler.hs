module HelVM.HelPS.HS2Lazy.Compiler where

import           HS2Lazy.PPrint ()
import           HS2Lazy.Syntax

import           Prelude        hiding (Alt, Ap)

skiCompile :: Expr -> SKI
skiCompile = compileExpr

compileExpr :: Expr -> SKI
compileExpr (Ap e1 e2) = compileExpr e1 `SAp` compileExpr e2
compileExpr (Let bg e) = compileLet bg e
compileExpr (Lambda a) = compileAlt a
compileExpr (Var i)    = SVar i
compileExpr (Lit l)    = SLit l
compileExpr (Con con)  = SCon (conTag con) (conArity con)
compileExpr e          = error ("compileExpr: " <> show e)

compileLet :: BindGroup -> Expr -> SKI
compileLet bg e = case map compileDef (bindings bg) of
  [(i, v)] -> case abstract i e' of
    SVar "K" `SAp` _ -> e'
    e''              -> e'' `SAp` removeSelfRec i v
  defs     -> compileMultipleDefs e' defs
  where e' = compileExpr e

compileDef :: (Id , [Alt]) -> (Id , SKI)
compileDef (i , [a]) = (i , compileAlt a)
compileDef _         = error "compileDef"

removeSelfRec :: Id -> SKI -> SKI
removeSelfRec i e
  | refers i e = SVar "Y" `SAp` abstract i e
  | otherwise  = e

compileMultipleDefs :: SKI -> [(Id, SKI)] -> SKI
compileMultipleDefs e defs
    | not $ any (flip refers e . fst) defs = e
    | otherwise = SAp lhs rhs
    where
      (is, vals) = unzip defs
      lhs = uAbs is e
      rhs = SVar "Y" `SAp` uAbs is (mklist vals)

mklist :: [SKI] -> SKI
mklist = foldr f (SVar "nil") where f x = SAp (SVar "cons" `SAp` x)

uAbs :: [Id] -> SKI -> SKI
uAbs [] e       = SVar "K" `SAp` e
uAbs (i : is) e = SVar "U" `SAp` abstract i (uAbs is e)

compileAlt :: Alt -> SKI
compileAlt ([] , Rhs e)      = compileExpr e
compileAlt (PVar v : as , e) = abstract v (compileAlt (as , e))
compileAlt (p : _ , _)       = error ("malformed pattern " <> show p)
compileAlt _                 = error "compileAlt"

abstract :: Id -> SKI -> SKI
abstract i v@(SVar i')
  | i == i' = SVar "I"
  | otherwise = SVar "K" `SAp` v
abstract i (SAp e1 e2)
  | refers i e1 || refers i e2 = sap (SVar "S") [abstract i e1 , abstract i e2]
  | otherwise = SAp (SVar "K") (SAp e1 e2)
abstract _ l@(SLit _) = SVar "K" `SAp` l
abstract _ c@(SCon _ _) = SVar "K" `SAp` c

refers :: Id -> SKI -> Bool
refers i (SVar i')   = i == i'
refers i (SAp e1 e2) = refers i e1 || refers i e2
refers _ (SLit _)    = False
refers _ (SCon _ _)  = False
