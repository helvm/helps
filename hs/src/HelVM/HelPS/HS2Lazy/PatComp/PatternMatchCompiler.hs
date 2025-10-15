module HelVM.HelPS.HS2Lazy.PatComp.PatternMatchCompiler (
    compilePatternMatch
  , patBindings
) where

import           HelVM.HelPS.HS2Lazy.Unsafe

import           HS2Lazy.PPrint             ()
import           HS2Lazy.Syntax

import           Data.List                  (partition)

import           Prelude                    hiding (Alt, Ap, Const)
import qualified Relude.Unsafe              as Unsafe


type PatComp = State Int

compilePatternMatch :: Program -> Program
compilePatternMatch pgm = evalState (pcProgram pgm) 0

pcProgram :: Program -> PatComp Program
pcProgram = traverse pcBindGroup

pcBindGroup :: BindGroup -> PatComp BindGroup
pcBindGroup (es, iss) = (,) <$> traverse pcExpl es <*> traverse (traverse pcImpl) iss

pcExpl :: Expl -> PatComp Expl
pcExpl (i, sc, alts) = go <$> pcAlts alts where go alt = (i, sc, [alt])

pcImpl :: Impl -> PatComp Impl
pcImpl (i, alts) = go <$> pcAlts alts where go alt = (i, [alt])

pcAlts :: [Alt] -> PatComp Alt
pcAlts [(ps, Rhs e)] | all isPVar ps = go <$> pcExpr e where go e' = (ps , Rhs e')
pcAlts qs = go =<< newVars (length $ fst $ unsafeHead qs) where
  go us = go' <$> match us qs matchError where
    go' rhs = (map PVar us , Rhs rhs)

isPVar :: Pat -> Bool
isPVar (PVar _) = True
isPVar _        = False

pcRhs :: Rhs -> Expr -> PatComp Expr
pcRhs (Rhs e) _          = pcExpr e
pcRhs (Where bg rhs) def = fmap (Let bg) (pcRhs rhs def)
pcRhs (Guarded gds) def  = foldr makeIf def <$> traverse pcGuard gds

pcGuard :: (Expr, Expr) -> PatComp (Expr, Expr)
pcGuard (e1, e2) = (,) <$> pcExpr e1 <*> pcExpr e2

pcExpr :: Expr -> PatComp Expr
pcExpr (Ap e1 e2) = Ap <$> pcExpr e1 <*> pcExpr e2
pcExpr (Let bg e) = Let <$> pcBindGroup bg <*> pcExpr e
pcExpr (Lambda a) = fmap Lambda (pcAlts [a])
pcExpr (Case e pes) = go =<< pcExpr e where
  go (Var v) = match' v
  go e''     = goLet e'' =<< newVar
  goLet e'' v = Let (bind1 v e'') <$> match' v
  match' v' = match [v'] qs matchError
  qs = [([p] , rhs) | (p , rhs) <- pes]
pcExpr (ESign e sc) = ESign <$> pcExpr e <*> pure sc
pcExpr c = pure c

matchError :: Expr
matchError = Ap (Var "error") (Lit $ LitStr "Non-exhaustive patterns")

patBindings :: Expr -> Pat -> [Impl]
patBindings v (PVar i)        = [(i, [([], Rhs v)])]
patBindings _ PWildcard       = []
patBindings v (PAs i p)       = (i, [([], Rhs v)]) : patBindings v p
patBindings _ (PLit _)        = []
patBindings v (PCon con pats) = concat [patBindings (makeSel con n v) p | (p, n) <- zip pats [1..]]

makeSel :: Const -> Int -> Expr -> Expr
makeSel con i e = expr where
  vs = ["@@" ++ show v | v <- [1 .. (conArity con)]]
  body = Rhs $ Var $ vs Unsafe.!! (i - 1)
  receiver' = receiver vs body
  expr = ap e [ifReceiver i' con receiver' eError | i' <- [1 .. (tyconNumCon $ conTycon con)]]

type Equation = Alt

isVar :: Equation -> Bool
isVar (p : _ , _) = test p where
  test (PVar _)   = True
  test PWildcard  = True
  test (PAs _ p') = test p'
  test (PLit _)   = False
  test (PCon _ _) = False
isVar _ = error "isVar"

match :: [Id] -> [Equation] -> Expr -> PatComp Expr
match [] qs def = foldrM pcRhs def (map snd qs)
match us qs def = foldrM (matchVarCon us) def (partitionEqns isVar qs)

matchVarCon :: [Id] -> [Equation] -> Expr -> PatComp Expr
matchVarCon us@(u : _) qs def = go $ unsafeHead $ fst $ unsafeHead qs' where
  go (PLit _)   = bindDefault (matchLit us qs') def
  go (PCon _ _) = bindDefault (matchCon us qs') def
  go _          = matchVar us qs' def
  qs' = map sub qs
  sub (PAs v p : ps, rhs) = sub (p : ps, Where (bind1 v (Var u)) rhs)
  sub (ps, rhs)           = (ps, rhs)
matchVarCon _ _ _ = error "matchVarCon"

matchVar :: [Id] -> [Equation] -> Expr -> PatComp Expr
matchVar (u : us) qs def = match us (map sub qs) def where
  sub (PVar v : ps , rhs)    = (ps , Where (bind1 v (Var u)) rhs)
  sub (PWildcard : ps , rhs) = (ps , rhs)
  sub _                      = error "sub"
matchVar _ _ _ = error "matchVar"

bindDefault :: (Expr -> PatComp Expr) -> Expr -> PatComp Expr
bindDefault f def
 | simple def = f def
 | otherwise  = go =<< newVar
  where
    go v = Let (bind1 v def) <$> f (Var v)
    simple _ = True

matchLit :: [Id] -> [Equation] -> Expr -> PatComp Expr
matchLit us qs def = foldr makeIf def <$> traverse (matchLitClause us def) (groupLit qs)

matchLitClause :: [Id] -> Expr -> (Literal, [Equation]) -> PatComp (Expr, Expr)
matchLitClause (u : us) def (lit , qs) = go <$> match us [(ps , rhs) | (_ : ps , rhs) <- qs] def
  where go e = (ap (Var "&eq") [Var u , Lit lit] , e)
matchLitClause _ _ _ = error "matchLitClause"

groupLit :: [Equation] -> [(Literal, [Equation])]
groupLit [] = []
groupLit qs@((PLit l:_,_):_) = (l, qs') : groupLit qs'' where
  (qs', qs'') = partition go qs
  go (PLit l' : _ , _) =  l == l'
  go _                 = False
groupLit _ = error "groupLit"

matchCon :: [Id] -> [Equation] -> Expr -> PatComp Expr
matchCon us qs def
  | isCovered grps = flip (foldr makeIf) <$> clauses <*> lastClause
  | otherwise = foldr makeIf def <$> traverse (matchConClause us def) grps
  where
    clauses = traverse (matchConClause us def) (Unsafe.init grps)
    lastClause = matchConLastClause us def (Unsafe.last grps)
    grps = groupCon qs

matchConClause :: [Id] -> Expr -> (Const, [Equation]) -> PatComp (Expr, Expr)
matchConClause (u : us) def (con, qs) = go =<< newVars (conArity con) where
  go us' = go' <$> match (us' ++ us) [(ps' ++ ps, rhs) | (PCon _ ps' : ps , rhs) <- qs] def where
    go' body = (cond, expr) where
      cond = makeTagEq con (Var u)
      expr = ap (Var u) [ifReceiver i con receiver' eError | i <- [1 .. (tyconNumCon $ conTycon con)]]
      receiver' = receiver us' $ Rhs body
matchConClause _ _ _ = error "matchConClause"

matchConLastClause :: [Id] -> Expr -> (Const, [Equation]) -> PatComp Expr
matchConLastClause us def grp = snd <$> matchConClause us def grp

groupCon :: [Equation] -> [(Const, [Equation])]
groupCon []                           = []
groupCon qs@((PCon c _ : _  , _) : _) = (c , qs') : groupCon qs'' where
  (qs' , qs'') = partition go qs
  go (PCon c' _ : _ , _) = c == c'
  go _                   = False
groupCon _                            = error "groupCon"

isCovered :: [(Const, [Equation])] -> Bool
isCovered grps = go $ tyconNumCon $ conTycon $ fst $ unsafeHead grps where
  go n = n == 0 || length grps == n

newVars :: Int -> PatComp [Id]
newVars k = go =<< get where go n = put (n + k) $> ['@' : show (n + i) | i <- [1 .. k]]

newVar :: PatComp Id
newVar = go =<< get where go n = put (n + 1) $> ('@' : show (n + 1))

makeIf :: (Expr , Expr) -> Expr -> Expr
makeIf (c , e) e' = ap (Var "IF") [c , e , e']

makeTagEq :: Const -> Expr -> Expr
makeTagEq con e = ap e es where
  arities = tyconArities (conTycon con)
  es = [test a (b == conTag con) | (a, b) <- zip arities [1..]]
  test arity b = Lambda ([PVar ('_':show n) | n <- [1 .. arity]] , Rhs $ ifThenElse b eTrue eFalse)

bind1 :: Id -> Expr -> BindGroup
bind1 v e = ([], [[(v, [([], Rhs e)])]])

ifReceiver :: Int -> Const -> p -> p -> p
ifReceiver i con = ifThenElse $ i == conTag con

receiver :: [Id] -> Rhs -> Expr
receiver vs body = Lambda ([PVar v | v <- vs] , body)

eError :: Expr
eError = Ap (Var "error") (Lit $ LitStr "!?")

----

partitionEqns :: Eq b => (a -> b) -> [a] -> [[a]]
partitionEqns _ [] = []
partitionEqns _ [x] = [[x]]
partitionEqns f (x : xs@(x' : _))
  | f x == f x' = tack x (partitionEqns f xs)
  | otherwise   = [x] : partitionEqns f xs

foldrM :: Monad m => (b -> a -> m a) -> a -> [b] -> m a
foldrM f a xs = foldlM (flip f) a (reverse xs)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y
