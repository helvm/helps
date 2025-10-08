module HelVM.HelPS.HS2Lazy.Optimizer (optimizeExpr) where

import           HelVM.HelIO.Control.Safe

import           HS2Lazy.Syntax

import           Prelude                  hiding (Alt, Ap, Const, Type, join, lift, lookupEnv, modify, newTVar)

optimizeExpr ::  MonadSafe m => Expr -> m Expr
optimizeExpr = optExpr

optExpr :: MonadSafe m => Expr -> m Expr
optExpr e@(Var _)             = pure e
optExpr e@(Lit _)             = pure e
optExpr e@(Con _)             = pure e
optExpr (Ap e1 e2)            = Ap <$> optExpr e1 <*> optExpr e2
optExpr (Let bg e)            = optLet bg e
optExpr (Lambda (vs , Rhs e)) = f <$> optExpr e where f r = Lambda (vs , Rhs r)
optExpr (ESign e _)           = optExpr e
optExpr _                     = liftError "optExpr"

optLet :: MonadSafe m => BindGroup -> Expr -> m Expr
optLet bg e = go $ bindings bg where
  go [(v , [([] , Rhs e')])] | simple e' = optExpr $ substVar e' v e
  go is                                  = Let <$> optBindGroup is <*> optExpr e

optBindGroup :: MonadSafe m => [Impl] -> m BindGroup
optBindGroup = fmap (([],) . pure) . traverse optImpl

optImpl :: MonadSafe m => Impl -> m Impl
optImpl (i, [(vs, Rhs e)]) = optImpl' <$> optExpr e where optImpl' e' = (i, [(vs, Rhs e')])
optImpl other              = pure other

simple :: Expr -> Bool
simple (Var _) = True
simple (Lit _) = True
simple (Con _) = True
simple _       = False

substVar :: Expr -> Id -> Expr -> Expr
substVar e' v e@(Var i)
  | i == v = e'
  | otherwise = e
substVar _ _ e@(Lit _) = e
substVar _ _ e@(Con _) = e
substVar e' v (Ap e1 e2) = Ap (substVar e' v e1) (substVar e' v e2)
substVar e' v (Let bg e)
  | v `elem` map fst (bindings bg) = Let bg e
  | otherwise = Let (subVarBindGroup e' v bg) (substVar e' v e)
substVar e' v (Case e gds) = Case (substVar e' v e) [(p , substVarCase e' v p rhs) | (p, rhs) <- gds]
substVar e' v (Lambda alt) = Lambda (subVarAlt e' v alt)
substVar e' v (ESign e sc) = ESign (substVar e' v e) sc
substVar _ _ (RecPH _)  = error "RecPH"
substVar _ _ (ClassPH _) = error "ClassPH"

substVarCase :: Expr -> Id -> Pat -> Rhs -> Rhs
substVarCase e' v p rhs = go $ v `elem` patVars p where
  go True  = rhs
  go False = subVarRhs e' v rhs

subVarBindGroup :: Expr -> Id -> BindGroup -> BindGroup
subVarBindGroup e' v (es, iss) = (es', iss') where
  es' = [(i, sc, subVarAlts e' v alts) | (i, sc, alts) <- es]
  iss' = map (\is -> [(i, subVarAlts e' v alts) | (i, alts) <- is]) iss

subVarAlts :: Expr -> Id -> [Alt] -> [Alt]
subVarAlts e' v = map (subVarAlt e' v)
subVarAlt :: Expr -> Id -> Alt -> Alt
subVarAlt e' v (ps , rhs)
  | any (elem v . patVars) ps = (ps , rhs)
  | otherwise = (ps , subVarRhs e' v rhs)

subVarRhs :: Expr -> Id -> Rhs -> Rhs
subVarRhs e' v (Rhs e) = Rhs (substVar e' v e)
subVarRhs e' v (Guarded gds) = Guarded [(substVar e' v c , substVar e' v e) | (c , e) <- gds]
subVarRhs e' v (Where bg rhs)
  | v `elem` map fst (bindings bg) = Where bg rhs
  | otherwise = Where (subVarBindGroup e' v bg) (subVarRhs e' v rhs)
