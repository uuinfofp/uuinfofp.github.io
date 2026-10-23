-- Solutions for INFOFP Midterm 2026-2027
module Exam261008 where

-- Question 1

type Weighted a = [(a, Int)]

-- 1a
extractWeight :: Eq a => a -> Weighted a -> (Int, Weighted a)
extractWeight x xs = (sum a1, filter a2 xs)
  where
    a1 = [w | (y,w) <- xs, x == y]
    a2 = \(y,_) -> x /= y

-- 1b
ticket :: Int -> Weighted a -> Maybe a
ticket i _ | i < 0 = Nothing
ticket _ [] = Nothing
ticket i ((x,w):xs)
  | i < w     = Just x
  | otherwise = ticket (i-w) xs

-- 1c
uncompress :: Weighted a -> [a]
uncompress = foldr (\(x,w) acc -> replicate w x ++ acc) []

-- 1d
andThen :: (a -> Weighted b) -> Weighted a -> Weighted b
andThen _ [] = []
andThen f ((x,w):xs) =
  let ys = f x
      rest = andThen f xs
  in map (\(y,v) -> (y,w*v)) ys ++ rest

-- 1e
coalesce :: Eq a => Weighted a -> Weighted a
coalesce = foldr step []
  where
    step (x,w) acc
      | w == 0 = acc
      | otherwise =
          let (v,rest) = extractWeight x acc
          in (x,w+v) : rest

data WeightedN a = W (Weighted a) deriving Show

instance Eq a => Eq (WeightedN a) where
  W xs == W ys = coalesce xs == coalesce ys


-- Question 2

data Trace a = Input a Float
             | Constant Float
             | Multiply Float (Trace a) (Trace a)
             deriving Show

value :: Trace a -> Float
value (Input _ v) = v
value (Constant c) = c
value (Multiply n _ _) = n

demo :: Trace Char
demo = Multiply 12 (Multiply 6 (Input 'x' 2) (Constant 3)) (Input 'x' 2)

-- 2a
inputs :: Trace a -> [a]
inputs (Input x _) = [x]
inputs (Constant _) = []
inputs (Multiply _ l r) = inputs l ++ inputs r

-- 2b
mulTrace :: Trace a -> Trace a -> Trace a
mulTrace (Constant 1) r = r
mulTrace l (Constant 1) = l
mulTrace l r = Multiply (value l * value r) l r

-- 2c
replay :: (a -> Float) -> Trace a -> Trace a
replay env (Input x _) = Input x (env x)
replay _ (Constant c) = Constant c
replay env (Multiply _ l r) =
  let l' = replay env l
      r' = replay env r
  in Multiply (value l' * value r') l' r'

-- 2d
inputs' :: Trace a -> [a]
inputs' t = go [] t
  where
    go acc (Input x _) = x : acc
    go acc (Constant _) = acc
    go acc (Multiply _ l r) = go (go acc r) l

-- 2e
gradient' :: Float -> Trace a -> Trace (a, Float)
gradient' _ (Constant c) = Constant c
gradient' s (Input x v) = Input (x,s) v
gradient' s (Multiply n l r) =
  Multiply n (gradient' (s * value r) l)
             (gradient' (s * value l) r)

gradient :: Trace a -> [(a,Float)]
gradient t = inputs' (gradient' 1 t)


-- Question 3

data Search a = Dead | Found a | Split (Search a) (Search a)

-- 3a
-- Split (Found ['x']) Dead :: Search [Char]
-- Found (Left Nothing)    :: Search (Either (Maybe a) b)
-- Found (map Just)        :: Search ([a] -> [Maybe a])
-- Split (Found []) (Found True): ill-typed; [a] cannot unify with Bool.

-- 3b
-- rerun f x = f x (f x): ill-typed.
-- If f x :: t, applying it to itself requires t ~ t -> u (occurs check).

collect :: (a -> b) -> [[a]] -> [b]
collect f xs = concatMap (map f) xs

agree :: Eq b => (a -> b) -> a -> a -> Bool
agree f x y = f x == f y

compareAt :: (Num a, Eq b) => (a -> b) -> a -> a -> Bool
compareAt f x w = f (x+w) == f x

-- 3c
{-
With fresh type variables:
  on        :: (b -> b -> c) -> (a -> b) -> a -> a -> c
  replicate :: Int -> d -> [d]

Thus on replicate :: (a -> b) -> a -> a -> c, subject to
  (b -> b -> c) ~ (Int -> d -> [d]).

Decomposing:
  b ~ Int
  b ~ d
  c ~ [d]

Hence b ~ Int, d ~ Int, c ~ [Int]. Substituting:
  on replicate :: (a -> Int) -> a -> a -> [Int]
-}


-- Question 4
-- a. Incorrect: the left-hand side is [('a',2),('b',0)].
-- b. Incorrect: the left-hand side is [1,2,0,0].
-- c. Correct:   the left-hand side is 7.
-- d. Incorrect: the left-hand side is [('x',8),('x',15)].
-- e. Correct:   the left-hand side is 9.
