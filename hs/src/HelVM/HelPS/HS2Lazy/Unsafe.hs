module HelVM.HelPS.HS2Lazy.Unsafe where

tack :: a -> [[a]] -> [[a]]
tack x xss = (x : unsafeHead xss) : unsafeTail xss

unsafeHead :: [a] -> a
unsafeHead []      = error "unsafeHead: empty list"
unsafeHead (a : _) = a

unsafeTail :: [a] -> [a]
unsafeTail []       = error "unsafeTail: empty list"
unsafeTail (_ : as) = as

--foldr1 :: (a -> a -> a) -> t a -> a
--foldr1 f xs = fromMaybe (error "foldr1: empty structure") (foldr mf Nothing xs) where
--  mf x m = Just (go m) where
--    go Nothing  = x
--    go (Just y) = f x y
--
--
--foldl1 :: (a -> a -> a) -> t a -> a
--foldl1 f xs = fromMaybe (error "foldl1: empty structure") (foldl' mf Nothing xs) where
--  mf m y = Just (go m) where
--    go Nothing  = y
--    go (Just x) = f x y

--foldl :: (b -> a -> b) -> b -> t a -> b
--foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
