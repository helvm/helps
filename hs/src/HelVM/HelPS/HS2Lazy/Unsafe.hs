module HelVM.HelPS.HS2Lazy.Unsafe where

tack :: a -> [[a]] -> [[a]]
tack x xss = (x : unsafeHead xss) : unsafeTail xss

unsafeHead :: [a] -> a
unsafeHead []      = error "unsafeHead: empty list"
unsafeHead (a : _) = a

unsafeTail :: [a] -> [a]
unsafeTail []       = error "unsafeTail: empty list"
unsafeTail (_ : as) = as
