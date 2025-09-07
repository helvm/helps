module HelVM.HelPS.HS2Lazy.Compiler where

import           HelVM.HelPS.HS2Lazy.PPrint            ()
import           HelVM.HelPS.HS2Lazy.Syntax
import           HelVM.HelPS.HS2Lazy.Syntax.Dependency

import qualified Relude.Unsafe                         as Unsafe

import           Prelude                               hiding (Alt, Ap, Const, Type, join, lift, lookupEnv, modify, newTVar)

programToExpr :: Program -> Expr
programToExpr bgs = foldr Let (mainExpr (Unsafe.last bgs)) bgs' where
  bgs' = regroup (Unsafe.init bgs)

mainExpr :: BindGroup -> Expr
mainExpr bg = go (bindings bg) where
    go [("@main" , [([] , Rhs e)])] = e
    go _                            = error "Illegal program entry point"

regroup :: [BindGroup] -> [BindGroup]
regroup bgs = [([] , [is]) | is <- iss] where
  iss = dependency (concatMap bindings bgs)

expandCon :: Expr -> Expr
expandCon e@(Var _)                = e
expandCon e@(Lit _)                = e
expandCon (Ap e1 e2)               = Ap (expandCon e1) (expandCon e2)
expandCon (Let bg e)               = Let (expandConBG bg) (expandCon e)
expandCon (Lambda (vs , Rhs e))    = Lambda (vs , Rhs (expandCon e))
expandCon (ESign e sc)             = ESign (expandCon e) sc
expandCon (Con con)                = conCon con
expandCon (Case _ _)               = error "Case"
expandCon (RecPH _)                = error "RecPH"
expandCon (ClassPH _)              = error "ClassPH"
expandCon (Lambda (_ , Where _ _)) = error "Lambda Where"
expandCon (Lambda (_ , Guarded _)) = error "Lambda Guarded"

conCon :: Const -> Expr
conCon con = Lambda ([PVar v | v <- as<>fs], Rhs body) where
  as = ["@a" <> show i | i <- [1 .. conArity con]]
  fs = ["@f" <> show i | i <- [1 .. (tyconNumCon $ conTycon con)]]
  body = ap (Var $ fs Unsafe.!! (tag - 1)) [Var v | v <- as]
  tag = calculateTag con

calculateTag :: Const -> Int
calculateTag con = go $ conTag con < 1 where
  go False = conTag con
  go True  = error $ "bad tag " <> toText (conName con)

expandConBG :: BindGroup -> BindGroup
expandConBG (es , iss) = (es' , map expandConImpls iss) where
  es' = [(i , sc , map expandConAlt alts) | (i , sc , alts) <- es]
  expandConImpls is = [(i , map expandConAlt alts) | (i , alts) <- is]
  expandConAlt (ps , rhs) = (ps , expandConRhs rhs)
  expandConRhs (Rhs e)         = Rhs (expandCon e)
  expandConRhs (Where bg rhs)  = Where (expandConBG bg) (expandConRhs rhs)
  expandConRhs (Guarded pairs) = Guarded [(expandCon c , expandCon e) | (c , e) <- pairs]

skiCompile :: Expr -> SKI
skiCompile = compileExpr

compileExpr :: Expr -> SKI
compileExpr (Ap e1 e2) = compileExpr e1 `SAp` compileExpr e2
compileExpr (Let bg e) = letBGE bg e
compileExpr (Lambda a) = compileAlt a
compileExpr (Var i)    = SVar i
compileExpr (Lit l)    = SLit l
compileExpr (Con con)  = SCon (conTag con) (conArity con)
compileExpr e          = error ("compileExpr: " <> show e)

letBGE :: BindGroup -> Expr -> SKI
letBGE bg e = go (map compileDef (bindings bg)) $ compileExpr e where
  go [(i , v)] e' = ive i v e'
  go defs     e'  = compileMultipleDefs e' defs

ive :: Id -> SKI -> SKI -> SKI
ive i v e' = go (abstract i e') where
  go (SVar "K" `SAp` _) = e'
  go e''                = e'' `SAp` removeSelfRec i v

compileDef :: (Id , [Alt]) -> (Id , SKI)
compileDef (i , [a]) = (i , compileAlt a)
compileDef _         = error "compileDef"

removeSelfRec :: Id -> SKI -> SKI
removeSelfRec i e
  | refers i e = SVar "Y" `SAp` abstract i e
  | otherwise  = e

compileMultipleDefs :: SKI -> [(Id , SKI)] -> SKI
compileMultipleDefs e defs
  | not $ any (flip refers e . fst) defs = e
  | otherwise = SAp lhs rhs
    where
      (is , vals) = unzip defs
      lhs = uAbs is e
      rhs = SVar "Y" `SAp` uAbs is (mklist vals)
      mklist []       = SVar "nil"
      mklist (x : xs) = SVar "cons" `SAp` x `SAp` mklist xs

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
