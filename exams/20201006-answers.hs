module Exam201001 where

import qualified Data.Map as Map

import Data.List
import Data.Maybe


--------------------------------------------------------------------------------
-- * 1. Eliminate multiples -- Programming, Recursion, List Comprehensions, Higher Order Functions
--------------------


-- (a) 
multipleFS :: Int -> Bool
multipleFS n = n `mod` 5 == 0 || n `mod` 7 == 0

-- NOTE: The example output for b actually removes multiples of 3 and
-- 5 rather than 5 and 7. We changed the prhasing of the question at
-- some point but forgot to update the examples

-- (b) 
em1 :: [Int] -> [Int] 
em1 [] = [] 
em1 (x : xs) | multipleFS x = em1 xs 
             | otherwise = x : em1 xs

-- (c) 
em2 :: [Int] -> [Int] 
em2 xs = [x | x <- xs, not (multipleFS x)]

-- (d) 
em3 :: [Int] -> [Int] 
em3 = filter (not . multipleFS)
-- or em3 = foldr (\x r -> if multipleFS x then r else x : r) []


--------------------------------------------------------------------------------
-- * 2. A puzzle -- User defined data types
--------------------

data Op = Plus | Minus | Multiply | Divide
        deriving Enum

instance Show Op where 
    show Plus = "+"
    show Minus = "-"
    show Multiply = "*"
    show Divide = "/"

type Value = Rational
apply :: Op -> Value -> Value -> Maybe Value
apply Plus x y = Just (x + y)
apply Minus x y = Just (x - y)
apply Multiply x y = Just (x * y)
apply Divide x 0 = Nothing
apply Divide x y = Just (x / y)

-- (a)
data Expr a = Const a | Binary Op (Expr a) (Expr a)

-- (b)
instance Show a => Show (Expr a) where 
    show (Const i)       = show i 
    show (Binary o e e') = "(" ++ show e ++ show o ++ show e' ++ ")"

-- (c) -- NB: there was a typo in the exam here -- the spec of splits should read:
-- (x:xs, y:ys) `elem` splits zs if and only if (x:xs) ++ (y:ys) = zs
-- This is part of why we have given everyone bonus points.
splits :: [a] -> [([a],[a])]
splits xs = map (split xs) [1..length xs - 1] where
    split xs n = (take n xs, drop n xs)


-- (d)
exprs :: [Integer] -> [(Expr Integer, Value)]
exprs [n] = [(Const n, fromInteger n)]
exprs ns  = [(Binary op e1 e2, v) | (ns1, ns2) <- splits ns,
                                      (e1, v1) <- exprs ns1,
                                      (e2, v2) <- exprs ns2,
                                            op <- [Plus, Minus, Multiply, Divide],
                                        Just v <- [apply op v1 v2]]

-- (e)
equalsFive :: [Integer] -> [Expr Integer]
equalsFive = map fst . filter ((==5) . snd) . exprs






--------------------------------------------------------------------------------
-- * 3. Multimaps
--------------------

type MultiMap k v = Map.Map k [v]
insertMap :: Ord k => k -> v -> Map.Map k v -> Map.Map k v
deleteMap :: Ord k => k -> Map.Map k a -> Map.Map k a
lookupMap :: Ord k => k -> Map.Map k a -> Maybe a
emptyMap :: Map.Map k a
insertMap = Map.insert 
deleteMap = Map.delete 
emptyMap = Map.empty 
lookupMap = Map.lookup

insert :: Ord k => k -> v -> MultiMap k v -> MultiMap k v
insert k v m = case lookupMap k m of
    Just vs -> insertMap k (v : vs) m
    Nothing -> insertMap k [v] m

delete :: Ord k => k -> MultiMap k a -> MultiMap k a
delete k m = case lookupMap k m of
    Just (_ : vs) -> insertMap k vs m
    _             -> deleteMap k m

lookup :: Ord k => k -> MultiMap k a -> Maybe a
lookup k m = case lookupMap k m of
    Just (v : _) -> Just v
    _             -> Nothing

empty :: MultiMap k a
empty = emptyMap

--------------------------------------------------------------------------------
-- * 4. Primitive recursion on natural numbers -- Folds
--------------------

-- We consider a type of natural numbers
data Nat = Zero | Succ Nat
-- We think of Zero as the number 0 and Succ n as the number n + 1.
-- On this, we can define a function
foldN :: (a -> a) -> a -> Nat -> a
foldN f e Zero     = e
foldN f e (Succ n) = f (foldN f e n)
-- analogous to foldr on [a].

data Dummy = D deriving Show


-- (a)
plus :: Nat -> Nat -> Nat
plus Zero n     = n
plus (Succ m) n = Succ (plus m n)
-- or plus (Succ m) n = plus m (Succ n)



-- (b)
mult :: Nat -> Nat -> Nat
mult n = foldN (plus n) Zero

-- (c)
listToNat :: [Dummy] -> Nat
listToNat = foldr (const Succ) Zero

-- (d)
natToList :: Nat -> [Dummy]
natToList = foldN (D:) []

-- (e)
mystery :: Nat -> Nat
-- is equivalent to the function
mystery Zero     = Zero 
mystery (Succ n) = n
-- hence, it sends "0" to "0" and "7" to "6"

--------------------------------------------------------------------------------
-- * 5. Multiple choice
--------------------
-- a.  1. B (incorrect) -- 5 is missing
--     2. B (incorrect) -- multiple copies of every element
--     3. A (correct)
--     4. A (correct)
--     5. A (correct)
--     6. B (incorrect)  -- 1 is missing
--     7. B (incorrect) -- 5 is missing

-- c.  1. A (Well-typed)
--     2. B (Ill-typed) -- cannot apply (\x -> x +1) to []
--     3. A (Well-typed)
--     4. A (Well-typed)
--     5. B (Ill-Typed) -- g needs to have a function type, i.e. a -> b, *and* g needs to have type 'a' (otherwise you cannot apply g to it.). So we need a ~ a -> b, which cannot be the case.

-- e.  1. B (incorrect) -- initial a too many

--     2. A (correct)
--     3. A (Correct)
--     4. B (Incorrect) -- doesn't typecheck
--     5. A (Incorrect) -- puts an separator at the end -> "xyz" -> "x,y,z,"
--     6. B (Incorrect) -- elements in the wrong order


--------------------------------------------------------------------------------
-- * 6. Type inference
--------------------

-- (a)

-- We have that 
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- const :: c -> d -> c 
-- id :: e -> e 
-- (Note: important to choose distinct type variables here.)

-- Observe that foldr const id should be read as (foldr const) id, as function 
-- application associates to the left.

-- Therefore, the types of const and id need to match the types of the 
-- first and second arguments of foldr, respectively.
-- That is,
-- c -> d -> c  ~  a -> b -> b  and b ~ e -> e
-- As long as those constraints are satisfied, we have that foldr const id has type [a] -> b
-- (where a and b need to satisfy the implied constraints)

-- These constraints are satisfied if and only if c ~ a, d ~ b, c ~ b and b ~ e -> e.
-- That is, iff a ~ b ~ c ~ d ~ e -> e. 
-- As [a] -> b ~ [e -> e] -> e -> e, it follows that foldr const id has type [e -> e] -> e -> e.


-- (b)

-- We have that 
-- flip :: (a -> b -> c) -> b -> a -> c
-- foldr :: (d -> e -> e) -> e -> ([d] -> e)
-- True :: Bool 
-- (&&) :: Bool -> Bool -> Bool 
-- (Note: important to choose distinct type variables here.)

-- Recall that flip foldr True (&&) needs to be read as ((flip foldr) True) (&&),
-- as function application associates to the left.

-- flip foldr True (&&) is well-typed with type c under the constraint that 
-- the type of foldr equals that of the first argument of flip, 
-- the type of True equals that of the second argument of flip,
-- and the type of (&&) equals that of the third argument of flip.  

-- That is, under the constraint that
-- a -> b -> c ~ (d -> e -> e) -> e -> ([d] -> e)
-- b ~ Bool 
-- a ~ Bool -> Bool -> Bool.

-- or, equivalently,
-- a ~ (d -> e -> e)
-- b ~ e 
-- c ~ [d] -> e 
-- b ~ Bool 
-- a ~ Bool -> Bool -> Bool.

-- or, equivalently still,
-- a ~ Bool -> Bool -> Bool 
-- b ~ e ~ Bool
-- d ~ Bool
-- c ~ [Bool] -> Bool.

-- As c ~ [Bool] -> Bool, if follows that flip foldr True (&&) has type [Bool] -> Bool


