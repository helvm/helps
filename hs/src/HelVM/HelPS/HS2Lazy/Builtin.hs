module HelVM.HelPS.HS2Lazy.Builtin (expandBltin) where

import           HelVM.HelIO.Control.Safe

import qualified Data.Map                 as Map

import           HS2Lazy.Syntax

expandBltin :: MonadSafe m => SKI -> m SKI
expandBltin (SAp (SVar "error") _)   = pure skiError
expandBltin (SAp e1 e2)              = SAp <$> expandBltin e1 <*> expandBltin e2
expandBltin (SVar v)                 = pure $ fromMaybe (SVar v) $ Map.lookup v builtins
expandBltin (SLit (LitInt n))        = liftMaybeOrError "expandBltin n" $ churchnums !!? n
expandBltin (SLit (LitChar (c : _))) = liftMaybeOrError "expandBltin c" $ churchnums !!? ord c
expandBltin _                        = liftError "expandBltin"

skiError :: SKI
skiError = SVar "I"

builtins :: Map Id SKI
builtins = fromList [
    ("+" , str"``si`k`s``s`ksk")
  , ("-" , str"``s`k`s`k```sii``s``s`ks``s`k`s``si`k`kk``s`k`s`k`s``s`ksk``s``s`ks``s`kk``sii`k``s``s``si`k`kk``si`kii`k`k`ki``s`k`s``si`k``s``s``si`k`kk``si`kii``s`kk``s``si`k``s`k`sik`k`k`ki")
  , ("*" , str"``s`ksk")
  , ("div" , str"``s`k`s```ss`s``s`ks``s`kk``s`k`s``s`ks``s`kk``s``s`ks``s`k`s``si`k`kk`s`k`s``s`ksk`k`k`ki``s`kk``si`k``s``s``si`k`kk``si`kii`k``sii``s`kk``s`k`si``s`kk``s``si`k``s`k`sik`k`k`ki")
  , ("mod" , str"``s`k`s```ss`s``s`ks``s`kk``s`k`s``s`ks``s``s`ks``s`kk``s`ks`s``si`k`kk`k``s`kk``s`k```sii``s``s`ks``s`k`s``si`k`kk``s`k`s`k`s``s`ksk``s``s`ks``s`kk``sii`k``s``s``si`k`kk``si`kii`k`k`ki``s``s``si`k`kk``si`kii``s`kk``si`k``s``s``si`k`kk``si`kii`k``sii``s`kk``s`k`si``s`kk``s``si`k``s`k`sik`k`k`ki")
  , ("&eq" , str"``s`k`s`k``si`kk``s`k`s``si`k``si`k`ki``s`kk``s``si`k``s`k`s``si`k`kik`k``s``si`kk`k```sii``s`k``s`k`s``si`k`kik``sii")
  , ("&neq" , str"``s`k`s`k``si`kk``s`k`s``si`k``si`k`ki``s`kk``s``si`k``s`k`s``si`kkk`k``s``si`k`ki`k```sii``s`k``s`k`s``si`kkk``sii")
  , ("<=" , str"``s``s`ks``s`kk``s``si`k``s`k`sik`k`kk`k``s``si`k``s`k`sik`k`k`ki")
  , (">=" , str"``s`k`s``s``si`k``s`k`sik`k`kk``s`kk``s``si`k``s`k`sik`k`k`ki")
  , ("<" , str"``s`k`s``s``si`k``s`k`sik`k`k`ki``s`kk``s``si`k``s`k`sik`k`kk")
  , (">" , str"``s``s`ks``s`kk``s``si`k``s`k`sik`k`k`ki`k``s``si`k``s`k`sik`k`kk")
  , ("&&" , str"``ss`k`k`ki")
  , ("||" , str"``si`kk")
  , ("." , str"``s`ksk")
  , ("++" , str"```sii``s``s`ks``s`k`s`ks``s`k`s``s`ksk``s`kk``s`k`s`k`s``s`ks``s`kk``s`k`s`k`s`kk``s``s`ks``s`kk``s`ks``s`k`sik`kk``s`k`s`kk``s``s`ks``s`kk``s`ks``sii`kk`k`ki")
  , ("Y" , str"```ss`s``s`ksk`k``sii")
  , ("U" , str"``s``s`ks``s``s`ksk`k``si`kk`k``si`k`ki")
  , ("cons" , str"``s``s`ks``s`kk``s`ks``s`k`sik`kk")
  , ("nil" , str"`kk")
  , ("IF" , SVar "I")
  , ("ord" , SVar "I")
  , ("chr" , SVar "I")
  ]

churchnums :: [SKI]
churchnums = [
    str "`ki" -- 0
  , str "i" -- 1
  , str "``s``s`kski" -- 2
  , str "``s``s`ksk``s``s`kski" -- 3
  , str "```sii``s``s`kski" -- 4
  , str "``s``s`ksk```sii``s``s`kski" -- 5
  , str "``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 6
  , str "``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 7
  , str "``s`k``s``s`kski```sii``s``s`kski" -- 8
  , str "```s``s`kski``s``s`ksk``s``s`kski" -- 9
  , str "``s`k``s``s`kski``s``s`ksk```sii``s``s`kski" -- 10
  , str "````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski" -- 11
  , str "``s`k``s``s`ksk``s``s`kski```sii``s``s`kski" -- 12
  , str "````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski" -- 13
  , str "``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski" -- 14
  , str "``s`k``s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 15
  , str "```s``siii``s``s`kski" -- 16
  , str "``s``s`ksk```s``siii``s``s`kski" -- 17
  , str "``s``s`ksk``s``s`ksk```s``siii``s``s`kski" -- 18
  , str "``s``s`ksk``s``s`ksk``s``s`ksk```s``siii``s``s`kski" -- 19
  , str "`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 20
  , str "``s``s`ksk`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 21
  , str "``s`k``s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski" -- 22
  , str "````s``s`kski`````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski`s``s`kski" -- 23
  , str "````s``s`kski````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 24
  , str "```s``s`kski``s``s`ksk```sii``s``s`kski" -- 25
  , str "``s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski" -- 26
  , str "```sii``s``s`ksk``s``s`kski" -- 27
  , str "``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 28
  , str "``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 29
  , str "``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 30
  , str "`````sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 31
  , str "``s`k``s``s`kski```s``siii``s``s`kski" -- 32
  , str "````s``s`kski````s``siii``s``s`kski`s``s`kski" -- 33
  , str "``s`k``s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 34
  , str "````s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 35
  , str "```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 36
  , str "``s``s`ksk```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 37
  , str "``s`k``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```s``siii``s``s`kski" -- 38
  , str "````s``s`kski```s``s`ksk``s``s`ksk``s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 39
  , str "``s`k``s``s`kski`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 40
  , str "`````s``siii``s``s`kski`s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski" -- 41
  , str "``s`k``s``s`kski``s``s`ksk`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 42
  , str "`````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 43
  , str "``s``s`ksk`````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 44
  , str "``s`k``s``s`ksk```sii``s``s`kski```s``s`kski``s``s`ksk``s``s`kski" -- 45
  , str "````s``s`ksk```sii``s``s`kski````s``s`kski``s``s`ksk``s``s`kski`s``s`kski" -- 46
  , str "``s``s`ksk````s``s`ksk```sii``s``s`kski````s``s`kski``s``s`ksk``s``s`kski`s``s`kski" -- 47
  , str "``s`k``s``s`ksk``s``s`kski```s``siii``s``s`kski" -- 48
  , str "````s``s`ksk``s``s`kski````s``siii``s``s`kski`s``s`kski" -- 49
  , str "``s`k``s``s`kski```s``s`kski``s``s`ksk```sii``s``s`kski" -- 50
  , str "``s`k``s``s`ksk``s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 51
  , str "````s``s`ksk``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 52
  , str "``s``s`ksk````s``s`ksk``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 53
  , str "``s`k``s``s`kski```sii``s``s`ksk``s``s`kski" -- 54
  , str "````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski" -- 55
  , str "``s`k``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 56
  , str "````s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 57
  , str "``s`k``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 58
  , str "````s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 59
  , str "``s`k``s``s`ksk``s``s`kski`````sii``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 60
  , str "````s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 61
  , str "``s`k``s``s`kski`````sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 62
  , str "````s``s`kski``````sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 63
  , str "```s`s``s`ksk``sii``s``s`kski" -- 64
  , str "``s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 65
  , str "``s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 66
  , str "``s``s`ksk``s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 67
  , str "``s`k```sii``s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 68
  , str "`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 69
  , str "``s``s`ksk`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 70
  , str "``s``s`ksk``s``s`ksk`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 71
  , str "``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```s``siii``s``s`kski" -- 72
  , str "`````sii``s``s`kski```s``s`ksk``s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 73
  , str "``s`k``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 74
  , str "``s`k``s``s`ksk``s``s`kski```s``s`kski``s``s`ksk```sii``s``s`kski" -- 75
  , str "````s``s`ksk``s``s`kski````s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 76
  , str "``s``s`ksk````s``s`ksk``s``s`kski````s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 77
  , str "``s`k``s``s`ksk``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski" -- 78
  , str "````s``s`ksk``s``s`kski```s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 79
  , str "``s`k``s``s`ksk```sii``s``s`kski```s``siii``s``s`kski" -- 80
  , str "```s``sii`s``s`ksk``s``s`kski" -- 81
  , str "``s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 82
  , str "``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 83
  , str "``s``s`ksk``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 84
  , str "`````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 85
  , str "``s``s`ksk`````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 86
  , str "``s`k``s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 87
  , str "````s``s`ksk``s``s`kski```s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 88
  , str "````s``s`kski````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 89
  , str "`````s``s`kski``s``s`ksk``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 90
  , str "`````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 91
  , str "``s``s`ksk`````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 92
  , str "````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 93
  , str "``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 94
  , str "``s`k``s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```s``siii``s``s`kski" -- 95
  , str "``s`k``s``s`ksk``s``s`ksk```sii``s``s`kski```s``siii``s``s`kski" -- 96
  , str "`````s``siii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 97
  , str "``s`k``s``s`kski````s``s`ksk``s``s`kski````s``siii``s``s`kski`s``s`kski" -- 98
  , str "``s`k``s``s`ksk``s``s`kski````s``s`kski````s``siii``s``s`kski`s``s`kski" -- 99
  , str "```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski" -- 100
  , str "``s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski" -- 101
  , str "``s`k``s``s`ksk``s``s`ksk```sii``s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 102
  , str "````s``s`ksk``s``s`ksk```sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 103
  , str "``s`k```sii``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski" -- 104
  , str "`````sii``s``s`kski```s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 105
  , str "`````s``s`kski``s``s`ksk```sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 106
  , str "````s``s`ksk```sii``s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 107
  , str "``s`k```sii``s``s`kski```sii``s``s`ksk``s``s`kski" -- 108
  , str "`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski" -- 109
  , str "``s``s`ksk`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski" -- 110
  , str "``s``s`ksk``s``s`ksk`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski" -- 111
  , str "``s`k```sii``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 112
  , str "`````sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 113
  , str "``s``s`ksk`````sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 114
  , str "````s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 115
  , str "``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 116
  , str "`````sii``s``s`kski```s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 117
  , str "````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 118
  , str "``s`k``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 119
  , str "``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 120
  , str "```s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski" -- 121
  , str "``s``s`ksk```s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski" -- 122
  , str "``s``s`ksk``s``s`ksk```s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski" -- 123
  , str "`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 124
  , str "```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 125
  , str "``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 126
  , str "``s``s`ksk``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 127
  , str "``s`k``s``s`kski```s`s``s`ksk``sii``s``s`kski" -- 128
  , str "````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`kski" -- 129
  , str "``s`k``s``s`kski``s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 130
  , str "````s``s`kski```s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`kski" -- 131
  , str "``s`k```sii``s``s`kski````s``s`kski````s``siii``s``s`kski`s``s`kski" -- 132
  , str "`````sii``s``s`kski`````s``s`kski````s``siii``s``s`kski`s``s`kski`s``s`kski" -- 133
  , str "``s`k``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 134
  , str "``s`k``s``s`ksk```sii``s``s`kski```sii``s``s`ksk``s``s`kski" -- 135
  , str "````s``s`ksk```sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski" -- 136
  , str "``s``s`ksk````s``s`ksk```sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski" -- 137
  , str "``s`k``s``s`kski`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 138
  , str "````s``s`ksk```sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`ksk```sii``s``s`kski" -- 139
  , str "``s`k``s``s`ksk```sii``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 140
  , str "````s``s`ksk```sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 141
  , str "``s``s`ksk````s``s`ksk```sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 142
  , str "``s``s`ksk``s``s`ksk````s``s`ksk```sii``s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 143
  , str "```s``s`kski``s`k``s``s`ksk``s``s`kski```sii``s``s`kski" -- 144
  , str "``s``s`ksk```s``s`kski``s`k``s``s`ksk``s``s`kski```sii``s``s`kski" -- 145
  , str "``s``s`ksk``s``s`ksk```s``s`kski``s`k``s``s`ksk``s``s`kski```sii``s``s`kski" -- 146
  , str "``s`k``s``s`ksk``s``s`kski````s``s`ksk``s``s`kski````s``siii``s``s`kski`s``s`kski" -- 147
  , str "``s`k```sii``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 148
  , str "`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 149
  , str "``s`k``s``s`ksk``s``s`ksk```sii``s``s`kski```s``s`kski``s``s`ksk```sii``s``s`kski" -- 150
  , str "````s``s`ksk```sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 151
  , str "`````sii``s``s`ksk``s``s`kski`s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 152
  , str "``s`k```s``s`kski``s``s`ksk``s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 153
  , str "`````s``s`kski``s``s`ksk``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 154
  , str "````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 155
  , str "``s``s`ksk````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 156
  , str "````s``s`kski```s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 157
  , str "``s`k``s``s`kski````s``s`ksk``s``s`kski```s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 158
  , str "`````sii``s``s`kski`````s``s`kski````s``siii``s``s`kski`s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 159
  , str "``s`k``s``s`kski``s`k``s``s`ksk```sii``s``s`kski```s``siii``s``s`kski" -- 160
  , str "````s``s`kski```s``s`ksk```sii``s``s`kski````s``siii``s``s`kski`s``s`kski" -- 161
  , str "``s`k``s``s`kski```s``sii`s``s`ksk``s``s`kski" -- 162
  , str "````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`kski" -- 163
  , str "``s`k``s``s`kski``s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 164
  , str "````s``s`kski```s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`kski" -- 165
  , str "``s`k``s``s`kski``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 166
  , str "````s``s`kski```s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`kski" -- 167
  , str "``s`k``s``s`kski``s``s`ksk``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 168
  , str "```s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski" -- 169
  , str "``s``s`ksk```s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski" -- 170
  , str "``s``s`ksk``s``s`ksk```s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski" -- 171
  , str "``s`k```sii``s``s`kski`````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 172
  , str "`````sii``s``s`kski``````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 173
  , str "``s`k``s``s`ksk``s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 174
  , str "``s`k``s``s`ksk```sii``s``s`kski````s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 175
  , str "``s`k````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski```s``siii``s``s`kski" -- 176
  , str "``````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski````s``siii``s``s`kski`s``s`kski" -- 177
  , str "````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 178
  , str "``s``s`ksk````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`ksk```s``siii``s``s`kski" -- 179
  , str "``s`k``s``s`ksk```sii``s``s`kski```s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 180
  , str "````s``s`ksk```sii``s``s`kski````s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski`s``s`kski" -- 181
  , str "``s`k``s``s`kski`````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 182
  , str "````s``s`kski``````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`kski" -- 183
  , str "``s`k``s``s`kski``s``s`ksk`````sii``s``s`ksk``s``s`kski`s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 184
  , str "`````s``siii``s``s`kski`s``s`ksk```s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski" -- 185
  , str "``s`k``s``s`kski````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 186
  , str "``s`k````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 187
  , str "``````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 188
  , str "````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 189
  , str "``s``s`ksk````s``s`kski````s``sii`s``s`ksk``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 190
  , str "````s``s`kski```s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 191
  , str "``s`k``s``s`ksk``s``s`kski```s`s``s`ksk``sii``s``s`kski" -- 192
  , str "````s``s`ksk``s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`kski" -- 193
  , str "``s``s`ksk````s``s`ksk``s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`kski" -- 194
  , str "``s`k``s``s`ksk``s``s`kski``s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 195
  , str "```s``s`kski``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski" -- 196
  , str "``s``s`ksk```s``s`kski``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski" -- 197
  , str "``s`k``s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski" -- 198
  , str "````s``s`ksk``s``s`kski```s``s`ksk``s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`kski" -- 199
  , str "``s`k``s``s`kski```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski" -- 200
  , str "````s``s`kski````s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 201
  , str "``s`k``s``s`kski``s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski" -- 202
  , str "````s``s`kski```s``s`ksk```s``s`kski``s`k``s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 203
  , str "``s`k``s``s`ksk``s``s`kski``s`k```sii``s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 204
  , str "````s``s`ksk``s``s`kski````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 205
  , str "`````s``sii`s``s`ksk``s``s`kski`s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 206
  , str "``s`k``s``s`ksk``s``s`kski`````sii``s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 207
  , str "``s`k````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski```s``siii``s``s`kski" -- 208
  , str "````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 209
  , str "``s``s`ksk````s``s`kski````s`s``s`ksk``sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 210
  , str "````s``s`kski```s``s`ksk```s`s``s`ksk``sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 211
  , str "``s`k``s``s`kski`````s``s`kski``s``s`ksk```sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 212
  , str "`````sii``s``s`kski`````s``s`kski````s``siii``s``s`kski`s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 213
  , str "``s`k``s``s`kski````s``s`ksk```sii``s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 214
  , str "``s`k``s``s`ksk```sii``s``s`kski`````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 215
  , str "```s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 216
  , str "``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`kski" -- 217
  , str "``s`k``s``s`kski`````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski" -- 218
  , str "````s``s`kski``````sii``s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski" -- 219
  , str "``s`k```sii``s``s`kski````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski" -- 220
  , str "`````sii``s``s`kski`````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski" -- 221
  , str "``s``s`ksk`````sii``s``s`kski`````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski" -- 222
  , str "``s``s`ksk``s``s`ksk`````sii``s``s`kski`````s``s`kski````sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski" -- 223
  , str "``s`k``s``s`kski``s`k```sii``s``s`kski``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 224
  , str "```s``s`kski``s`k``s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 225
  , str "``s``s`ksk```s``s`kski``s`k``s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 226
  , str "``s``s`ksk``s``s`ksk```s``s`kski``s`k``s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 227
  , str "``s`k```sii``s``s`kski````s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 228
  , str "`````sii``s``s`kski`````s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski" -- 229
  , str "``s``s`ksk`````sii``s``s`kski`````s``s`kski```s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski`s``s`kski" -- 230
  , str "``s`k``s``s`ksk``s``s`ksk``s``s`ksk```sii``s``s`kski````s``s`kski````s``siii``s``s`kski`s``s`kski" -- 231
  , str "``s`k``s``s`kski``s`k```sii``s``s`kski``s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski" -- 232
  , str "````s``s`kski````sii``s``s`kski```s``s`ksk``s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 233
  , str "``s`k```s``s`kski``s``s`ksk``s``s`kski``s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski" -- 234
  , str "`````s``s`kski``s``s`ksk``s``s`kski```s``s`ksk```s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 235
  , str "``s`k```sii``s``s`kski````s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski" -- 236
  , str "`````sii``s``s`kski`````s``s`kski````s``siii``s``s`kski`s``s`ksk```sii``s``s`ksk``s``s`kski`s``s`kski" -- 237
  , str "``s`k``s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski``s``s`ksk```s``siii``s``s`kski" -- 238
  , str "````s``s`ksk````s``s`ksk``s``s`kski````sii``s``s`kski`s``s`kski```s``s`ksk```s``siii``s``s`kski`s``s`kski" -- 239
  , str "``s`k``s``s`ksk``s``s`kski``s`k``s``s`ksk```sii``s``s`kski```s``siii``s``s`kski" -- 240
  , str "````s``s`ksk``s``s`kski```s``s`ksk```sii``s``s`kski````s``siii``s``s`kski`s``s`kski" -- 241
  , str "``s`k``s``s`kski```s``s`kski````s``s`kski```s``s`ksk```sii``s``s`kski`s``s`kski" -- 242
  , str "```s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`kski" -- 243
  , str "``s``s`ksk```s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`kski" -- 244
  , str "``s``s`ksk``s``s`ksk```s``s`ksk```sii``s``s`kski``s``s`ksk``s``s`kski" -- 245
  , str "``s`k``s``s`ksk``s``s`kski``s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 246
  , str "````s``s`ksk``s``s`kski```s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`kski" -- 247
  , str "``s``s`ksk````s``s`ksk``s``s`kski```s``s`ksk```s``sii`s``s`ksk``s``s`kski`s``s`kski" -- 248
  , str "``s`k``s``s`ksk``s``s`kski``s``s`ksk``s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 249
  , str "``s`k``s``s`kski```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 250
  , str "````s``s`kski````s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 251
  , str "``s`k``s``s`kski``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 252
  , str "````s``s`kski```s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski`s``s`kski" -- 253
  , str "``s`k``s``s`kski``s``s`ksk``s``s`ksk```s``s`ksk``s``s`kski``s``s`ksk```sii``s``s`kski" -- 254
  , str "``s`k``s``s`ksk``s``s`kski`````sii``s``s`kski`s``s`ksk```s``sii`s``s`ksk``s``s`kski" -- 255
  , str "```sii```sii``s``s`kski" --256
  ]

str :: String -> SKI
str = SLit . LitStr
