
import Data.List(foldr,zipWith)
import Data.Char(chr)

permutations        :: [a] -> [[a]]
permutations []     = [[]]
permutations (x:xs) = concatMap (insertEverywhere x) $ permutations xs


insertEverywhere             :: a -> [a] -> [[a]]
insertEverywhere x []        = [[x]]
insertEverywhere x xs@(y:ys) = (x:xs) : map (y:) (insertEverywhere x ys)


-- permutations        :: [a] -> [[a]]
-- permutations = foldr f e
--   where
--     e     = [[]]
--     f x r = concatMap (insertEverywhere x) r


foo = foldr f "" [65..70]
  where
    f i s = chr i : s

-- pow2   :: Int -> Int
pow2 0 = 1
pow2 n = 2 * pow2 (n-1)


x `pow` 0 = 1
x `pow` n | even n    = let y = x `pow` (n `div` 2) in y * y
          | otherwise = x  * (x `pow` (n-1))


span' 0 xs     = ([],xs)
span' i []     = ([],[])
span' i (x:xs) = let (ys,rest) = span' (i-1) xs
                 in (x:ys,rest)

split i xs = case span' i xs of
               (ys,[])   -> [ys]
               (ys,rest) -> ys : split i rest




transpose [] = []
transpose xs = transpose' xs

transpose' []       = repeat []
transpose' (xs:xss) = zipWith (:) xs (transpose' xss)

encode        :: Eq a => [a] -> [(a,Int)]
encode []     = []
encode (x:xs) = case encode xs of
                  []                       -> [(x,1)]
                  r@((y,i):ys) | x == y    -> (y,i+1) : ys
                               | otherwise -> (x,1)   : r

decode :: [(a,Int)] -> [a]
decode = concatMap (\(x,i) -> replicate i x)



nub [] = []
nub (x:xs) | x `elem` xs = nub xs
           | otherwise   = x : nub xs
