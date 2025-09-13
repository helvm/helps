{-# OPTIONS_GHC -Wno-orphans #-}
module HelVM.HelPS.HS2Lazy.Syntax
  ( Alt
  , Assump (..)
  , BindGroup
  , Class
  , ClassEnv (..)
  , Const (..)
  , EnvTransformer
  , Expl
  , Expr (..)
  , Id
  , Impl
  , Inst
  , Kind (..)
  , Literal (..)
  , Pat (..)
  , Pred (..)
  , Program
  , Qual (..)
  , Rhs (..)
  , SKI (..)
  , Scheme (..)
  , Subst
  , Synonym (..)
  , Tycon (..)
  , Type (..)
  , Types (..)
  , Tyvar (..)
  , ap
  , assoc
  , bindings
  , eCons
  , eFalse
  , eNil
  , eTrue
  , findAssump
  , fn
  , fromTAp
  , fvAlts
--  , fvBindGroup
  , idEnvTransformer
  , kind
  , list
  , pCons
  , pNil
  , patVars
  , preludeConstrs
  , preludeSynonyms
  , preludeTycons
  , quantify
  , quantifyAll
  , quantifyAll'
  , sap
  , tBool
  , tChar
  , tInt
  , tString
  , toScheme
  , tupTycon
  , tupcon
  , tuple
  , tupleSelector
  , unsynonym
  , (<:>)
  , fvBindGroup
  , pair
  , tUnit
  )
  where

import           HS2Lazy.Syntax hiding (ap, bindings, dependency, eCons, eFalse, eNil, eTrue, findAssump, fn, fromTAp, fvAlt, fvAlts, fvBindGroup,
                                 idEnvTransformer, list, pCons, pNil, pair, patVars, preludeConstrs, preludeSynonyms, preludeTycons, quantify, quantifyAll,
                                 quantifyAll', sap, tArrow, tBool, tChar, tInt, tList, tString, tUnit, toScheme, tupTycon, tupcon, tuple, tupleSelector,
                                 unsynonym, (<:>))

import           Data.List      (foldl, foldl1, union, (\\))

import           Safe

import qualified Text.Show
--import           Text.Show      (ShowS, showParen, shows, showsPrec)
import           Text.Show      (shows, showsPrec)

import           Prelude        hiding (Alt, Ap, Const, Type)

fromTAp :: Type -> [Type]
fromTAp (TAp t1 t2) = fromTAp t1 ++ [t2]
fromTAp t           = [t]

unsynonym :: Synonym -> [Type] -> Type
unsynonym (Synonym _ _ vs t) ts = apply s t
    where s = zip vs ts

tChar :: Type
tChar    = TCon (Tycon "Char" Star 0 [])

tInt :: Type
tInt     = TCon (Tycon "Int" Star 0 [])

tBool :: Type
tBool    = TCon (Tycon "Bool" Star 2 [0,0])

tUnit :: Type
tUnit    = TCon (Tycon "()" Star 1 [0])

tList :: Type
tList    = TCon (Tycon "[]" (Kfun Star Star) 2 [2,0])

tArrow :: Type
tArrow   = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)) 0 [])

tString    :: Type
tString     = list tChar

preludeTycons :: [Tycon]
preludeTycons = [
  Tycon "()" Star 1 [0],
  Tycon "Char" Star 0 [],
  Tycon "Int" Star 0 [],
  Tycon "Bool" Star 2 [0,0],
  Tycon "[]" (Kfun Star Star) 2 [2,0],
  Tycon "(->)" (Kfun Star (Kfun Star Star)) 0 []
  ]

preludeSynonyms :: [Synonym]
preludeSynonyms = [Synonym "String" Star [] (list tChar)]

preludeConstrs :: [Const]
preludeConstrs =
  [ Const
      { conName   = i
      , conArity  = ar
      , conTag    = tag
      , conTycon  = tycon
      , conScheme = quantifyAll' t
      }
  | (i, ar, tag, TCon tycon, t) <- constrs
  ]
  where
    a :: Type
    a = TVar (Tyvar "a" Star)
    constrs :: [(String, Int, Int, Type, Type)]
    constrs =
      [ ("True",  0, 1, tBool,               tBool)
      , ("False", 0, 2, tBool,               tBool)
      , (":",     2, 1, tList,               a `fn` list a `fn` list a)
      , ("[]",    0, 2, tList,               list a)
      ]


eTrue :: Expr
eTrue = Con $ findConst "eTrue" "True"

eFalse :: Expr
eFalse = Con $ findConst "eFalse" "False"

eCons :: Expr
eCons = Con $ findConst "eCons" ":"

eNil :: Expr
eNil = Con $ findConst "eNil" "[]"

pCons :: Pat -> Pat -> Pat
pCons x y = flip PCon [x, y] $ findConst "pCons" ":"

pNil :: Pat
pNil = flip PCon [] $ findConst "pNil" "[]"

findConst :: String -> Id -> Const
findConst note name = fromJustNote note $ find ((== name) . conName) preludeConstrs

infixr      4 `fn`
fn         :: Type -> Type -> Type
a `fn` b    = TAp (TAp tArrow a) b

list       :: Type -> Type
list       = TAp tList

pair       :: Type -> Type -> Type
pair a b    = TCon (tupTycon 2) `fn` a `fn` b

quantify      :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt) where
  vs' = [ v | v <- tv qt, v `elem` vs ]
  ks  = map kind vs'
  s   = zip vs' (map TGen [0..])

quantifyAll :: Qual Type -> Scheme
quantifyAll t = quantify (tv t) t

quantifyAll' :: Type -> Scheme
quantifyAll' t = quantify (tv t) ([] :=> t)

toScheme      :: Type -> Scheme
toScheme t     = Forall [] ([] :=> t)

findAssump :: MonadFail m => Id -> [Assump] -> m Scheme
findAssump ide []                = fail ("unbound identifier: " ++ ide)
findAssump ide ((i :>: sc) : as) = if i == ide then return sc else findAssump ide as

ap :: Expr -> [Expr] -> Expr
ap = foldl Ap

bindings :: BindGroup -> [Impl]
bindings (es, iss) = [(i , as) | (i , _ , as) <- es] ++ concat iss

fvBindGroup :: BindGroup -> [Id]
fvBindGroup bg = fvAlts (concat altss) \\ is where
  (is, altss) = unzip (bindings bg)

fvAlts :: [Alt] -> [Id]
fvAlts alts = foldl1 union (map fvAlt alts)
fvAlt :: Alt -> [Id]
fvAlt (ps, rhs) = freeVars rhs \\ concatMap patVars ps

patVars :: Pat -> [Id]
patVars (PVar i)    = [i]
patVars (PAs i p)   = i : patVars p
patVars (PCon _ ps) = concatMap patVars ps
patVars _           = []

tupcon :: Int -> Const
tupcon n = Const "(,)" n 1 tycon sc where
  tycon = tupTycon n
  tuptype = foldl TAp (TCon tycon) tvars
  tvars = [TGen i | i <- [0 .. n - 1]]
  sc = Forall (replicate n Star) ([] :=> foldr fn tuptype tvars)

tupTycon :: Int -> Tycon
tupTycon n = Tycon "(,)" (foldr Kfun Star (replicate n Star)) 1 [0]

tuple :: [Expr] -> Expr
tuple es = foldl Ap (Con $ tupcon $ length es) es

tupleSelector :: String -> Int -> Int -> Impl
tupleSelector ide k n = (ide, [([pat], Rhs expr)]) where
  pat = PCon (tupcon n) [PVar ('e' : show i) | i <- [0..n-1]]
  expr = Var ('e' : show k)

idEnvTransformer :: EnvTransformer
idEnvTransformer = Just

infixr 5 <:>
(<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) = (>=>)

sap :: SKI -> [SKI] -> SKI
sap = foldl SAp

----

instance Show SKI where
    show e = showsPrec 1 e ""
    showsPrec _ (SVar i)    = (i++)
    showsPrec _ (SLit l)    = shows l
    showsPrec _ (SCon k n)  = ('@':) . shows k . ('_':) . shows n
    showsPrec _ (SAp e1 e2) = ('`':) . shows e1 . shows e2
