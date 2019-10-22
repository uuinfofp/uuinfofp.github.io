module Lecture4 where

-- derivative
differentiate :: (Double -> Double) -> Double -> Double
differentiate f a = (f (a + delta) - f a) / delta
  where
    delta = 0.00000001

-- map
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- applyAll
applyAll :: [a -> a] -> a -> a
applyAll [] a     = a
applyAll (f:fs) a = applyAll fs (f a)

-- filterNot
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter (not . p)

-- applyAll in pointfree style
applyAll' :: [a -> a] -> a -> a
applyAll' []     = id
applyAll' (f:fs) = applyAll fs . f

-- length, using foldr
length' :: [a] -> Int
length' = foldr (const (+ 1)) 0

-- count, using filter
count :: (a -> Bool) -> [a] -> Int
count p = length' . filter p

-- applyAll using foldl
applyAll2 :: [a -> a] -> a -> a
applyAll2 = flip (foldl (\acc f -> f acc))

-- applyAll using foldr
applyAll3 :: [a -> a] -> a -> a
applyAll3 = foldr (flip (.)) id
