module HelVM.HelPS.HS2Lazy.Compiler.SkiCompiler (skiCompile) where

import           HelVM.HelIO.Control.Safe

import           HS2Lazy.PPrint           ()
import           HS2Lazy.Syntax

import           Prelude                  hiding (Alt, Ap)

skiCompile :: MonadSafe m => Expr -> m SKI
skiCompile = compileExpr

compileExpr :: MonadSafe m => Expr -> m SKI
compileExpr (Ap e1 e2) = SAp <$> compileExpr e1 <*> compileExpr e2
compileExpr (Let bg e) = compileLet bg =<< compileExpr e
compileExpr (Lambda a) = compileAlt a
compileExpr (Var i)    = pure $ SVar i
compileExpr (Lit l)    = pure $ SLit l
compileExpr (Con con)  = pure $ SCon (conTag con) (conArity con)
compileExpr e          = liftError ("compileExpr: " <> show e)

compileLet :: MonadSafe m => BindGroup -> SKI -> m SKI
compileLet bg e = go <$> compileBindGroup bg where
  go [(i, v)] = compilePair e i v
  go defs     = compileMultipleDefs e defs

compileBindGroup :: MonadSafe m => BindGroup -> m [(Id, SKI)]
compileBindGroup bg = traverse compileDef $ bindings bg

compilePair :: SKI -> Id -> SKI -> SKI
compilePair e i v = go $ abstract i e where
  go (SVar "K" `SAp` _) = e
  go e'                 = e' `SAp` removeSelfRec i v

compileDef :: MonadSafe m => (Id , [Alt]) -> m (Id , SKI)
compileDef (i , [a]) = (i , ) <$> compileAlt a
compileDef _         = liftError "compileDef"

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

compileAlt :: MonadSafe m => Alt -> m SKI
compileAlt ([] , Rhs e)      = compileExpr e
compileAlt (PVar v : as , e) = abstract v <$> compileAlt (as , e)
compileAlt (p : _ , _)       = liftError ("malformed pattern " <> show p)
compileAlt _                 = liftError "compileAlt"

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
