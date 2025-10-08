module HelVM.HelPS.HS2Lazy.Compiler.ExpandCon (expandCon) where

import           HelVM.HelIO.Containers.LLIndexSafe
import           HelVM.HelIO.Control.Safe

import           HS2Lazy.Syntax

import           Prelude                            hiding (Alt, Ap, Const, Type, join, lift, lookupEnv, modify, newTVar)

expandCon :: MonadSafe m => Expr -> m Expr
expandCon e@(Var _)             = pure e
expandCon e@(Lit _)             = pure e
expandCon (Ap e1 e2)            = liftA2 Ap (expandCon e1) (expandCon e2)
expandCon (Let bg e)            = liftA2 Let (expandConBG bg) (expandCon e)
expandCon (Lambda (vs , Rhs e)) = buildLambda <$> expandCon e where buildLambda e' = Lambda (vs, Rhs e')
expandCon (ESign e sc)          = flip ESign sc <$> expandCon e
expandCon (Con con)             = conCon con
expandCon _                     = liftError "expandCon"

conCon :: MonadSafe m => Const -> m Expr
conCon con = makeExpr =<< calculateTag con where
  makeExpr tag = buildLambda <$> (indexSafe fs $ tag - 1) where
    buildLambda f = Lambda (map PVar (as <> fs), Rhs (ap (Var f) (Var <$> as)))
    as = a <$> [1 .. conArity con] where a i = "@a" <> show i
    fs = f <$> [1 .. tyconNumCon (conTycon con)] where f i = "@f" <> show i

calculateTag :: MonadSafe m => Const -> m Int
calculateTag con
  | t > 0     = pure t
  | otherwise = liftError $ "bad tag " <> toText (conName con)
  where
    t = conTag con

expandConBG :: MonadSafe m => BindGroup -> m BindGroup
expandConBG (es , iss) = (,) <$> expandConExpls es <*> expandConImplss iss

expandConExpls :: MonadSafe m => [Expl] -> m [Expl]
expandConExpls = traverse expandConExpl

expandConExpl :: MonadSafe m => Expl -> m Expl
expandConExpl (i , sc , alts) = (i , sc , ) <$> traverse expandConAlt alts

expandConImplss :: MonadSafe m => [[Impl]] -> m [[Impl]]
expandConImplss = traverse expandConImpls

expandConImpls :: MonadSafe m => [Impl] -> m [Impl]
expandConImpls = traverse expandConImpl

expandConImpl :: MonadSafe m => Impl -> m Impl
expandConImpl (i , alts) = (i, ) <$> traverse expandConAlt alts

expandConAlt :: MonadSafe m => Alt -> m Alt
expandConAlt (ps , rhs) = (ps, ) <$> expandConRhs rhs

expandConRhs :: MonadSafe m => Rhs -> m Rhs
expandConRhs (Rhs e)         = Rhs <$> expandCon e
expandConRhs (Where bg rhs)  = Where <$> expandConBG bg <*> expandConRhs rhs
expandConRhs (Guarded pairs) = Guarded <$> traverse expandConGuard pairs

expandConGuard :: MonadSafe m => (Expr , Expr) -> m (Expr , Expr)
expandConGuard (c , e) = (,) <$> expandCon c <*> expandCon e
