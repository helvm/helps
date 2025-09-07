module HelVM.HelPS.HS2Lazy.Syntax.Dependency (dependency) where

import           HelVM.HelPS.HS2Lazy.SCC
import           HelVM.HelPS.HS2Lazy.Syntax

import           Data.List                  (intersect, lookup)

import           Safe

import           Prelude                    hiding (Alt, Const, Type)

dependency :: [Impl] -> [[Impl]]
dependency bs = (map . map) (\v -> (v, lookupNote v bs)) (reverse vss) where
  vs = map fst bs
  vss = scc [(v, fvAlts alts `intersect` vs) | (v, alts) <- bs]
  lookupNote key xs = fromJustNote "cannot occur" $ lookup key xs
