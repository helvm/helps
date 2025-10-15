module HelVM.HelPS.HS2Lazy.Compiler.ProgramToExprConverter (programToExpr) where

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.ListLikeExtra

import           HS2Lazy.Syntax

programToExpr :: MonadSafe m => Program -> m Expr
programToExpr bgs = go <$> unsnocSafe bgs where
  go (initBG, lastBG) = foldr Let (mainExpr lastBG) (regroup initBG)

mainExpr :: BindGroup -> Expr
mainExpr bg = case bindings bg of
  [("@main", [([], Rhs e)])] -> e
  _                          -> error "Illegal program entry point"

regroup :: [BindGroup] -> [BindGroup]
regroup bgs = [([], [is]) | is <- iss] where
  iss = dependency $ concatMap bindings bgs
