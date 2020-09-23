module Lecture4 where

-- derivative
differentiate :: (Double -> Double) -> Double -> Double
differentiate f a = (f (a + delta) - f a) / delta where 
    delta = 0.000001






















-- Why not pattern match?
-- differentiate exp = exp 
-- differentiate sin = cos
-- etc.
-- We cannot pattern match on functions! They are black boxes that send inputs to outputs.
-- If we want to inspect their source code, we need to use meta-programming, which is beyond the scope of this course.
















-- map
map' :: (a -> b) -> [a] -> [b]
map' f []       = []
map' f (x : xs) = f x : map' f xs

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x : xs) | p x       = x : filter' p xs
                   | otherwise = filter' p xs

















-- flip, using lambda
flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \b a -> f a b 


















-- applyAll
applyAll :: [a -> a] -> a -> a
applyAll [] a = a
applyAll [f] a = f a 
applyAll (f : fs) a = applyAll fs (f a)
















-- filterNot
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p  = filter (not . p)
















-- applyAll in pointfree style
applyAll' :: [a -> a] -> a -> a
applyAll' []  = id
applyAll' (f  : fs) = applyAll' fs . f 















-- length, using foldr
length' :: [a] -> Int
length'  = foldr f v where 
    v = 0 
    f _ = (+ 1) 




-- monoid: type T
-- element v : T 
-- operation: f : T -> T -> T 
-- f x v = x = f v x 
-- f x (f y z) = f (f x y) z
 










-- count, using filter
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p
















-- applyAll using foldl
applyAll2 :: [a -> a] -> a -> a
applyAll2 fs x = foldl g v fs where 
    v = x
    g acc f = f acc
















-- applyAll using foldr
applyAll3 :: [a -> a] -> a -> a
applyAll3 fs = foldr g v fs where 
    v = id 
    g = flip (.)














-- map :: (a -> b) -> ([a] -> [b])
-- (+) :: Int -> (Int -> Int)
-- map (+) :: [a] -> [b]
-- where (a -> b) ~ Int -> (Int -> Int)
-- so, a ~ Int,  b ~ Int -> Int 
-- therefore, map (+) :: [Int] -> [Int -> Int]


-- foldl :: (b -> a -> b) -> b -> [a] -> b 
-- foldl f v [] = v 
-- foldl f v (x : xs) = foldl f (f v x) xs

-- reduce :: (a -> a -> a) -> [a] -> a      (e.g.  Python,   is a lot like a limited case of foldl)
-- reduce f v [] = error "Not defined! :-("
-- reduce f v (x : xs) = reduce f (f v x) xs