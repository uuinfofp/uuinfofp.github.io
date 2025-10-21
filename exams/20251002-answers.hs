{-# LANGUAGE PartialTypeSignatures #-}
module Exam251002 where

import           Prelude hiding (Either(..), Semigroup(..), Monoid(..), scanl, zipWith)

-- INFOFP – Functional Programming – Midterm exam
--
-- General clarifications (apply to the whole exam):
-- • Unless stated otherwise, list indices are 0-based.
-- • You may assume finite lists unless a question explicitly asks about infinite ones.
-- • Examples are illustrative, not test-complete; handle all valid inputs according to the spec.


-- Q1. List-based building blocks for polynomials (TOTAL: 20 pt)
--
-- We work with *lists of coefficients* where the i-th element is the
-- coefficient of x^i. Subparts (a)–(d) are purely about lists; only in (e)
-- we introduce the Poly type.


-------------------------------------------------------------------------------
-- Provided helper
-------------------------------------------------------------------------------

stripZeros :: (Eq a, Num a) => [a] -> [a]
stripZeros = reverse . dropWhile (== 0) . reverse

-------------------------------------------------------------------------------
-- (a) 3 pt — Basic programming (no recursion): degree of a coefficient list
-- MARKING (a): strips zeros 1, Nothing case 1, length-1 1.
-------------------------------------------------------------------------------

degreeList :: (Eq a, Num a) => [a] -> Maybe Int
degreeList cs =
  let nz = stripZeros cs
  in if null nz then Nothing else Just (length nz - 1)


-------------------------------------------------------------------------------
-- (b) 5 pt — Explicit recursion on lists: coefficient-wise addition with padding
-- MARKING (b): recursive step 3, base [] bs 1, base as [] 1.
-------------------------------------------------------------------------------

addListsPad :: Num a => [a] -> [a] -> [a]
addListsPad (a:as) (b:bs) = (a+b) : addListsPad as bs
addListsPad []     bs     = bs
addListsPad as     []     = as


-------------------------------------------------------------------------------
-- (c) 4 pt — Use of map & filter (no recursion here)
-- MARKING (c): uses map 1, filter 1, zip/indexing 1, composition/clarity 1.
-------------------------------------------------------------------------------

nonZeroPositions :: (Eq a, Num a) => [a] -> [Int]
nonZeroPositions cs =
  map snd (filter (\(c,_i) -> c /= 0) (zip cs [0..]))


-------------------------------------------------------------------------------
-- (d) 4 pt — Use of a fold: Horner evaluation on a coefficient list
-- MARKING (d): foldr 1, Horner step a + x*acc 2, initial 0 1.
-------------------------------------------------------------------------------

eval :: Num a => [a] -> a -> a
eval cs x = foldr (\a acc -> a + x*acc) 0 cs


-------------------------------------------------------------------------------
-- (e) 4 pt — Basic typeclass instance for polynomials
--
-------------------------------------------------------------------------------

newtype Poly a = P [a]
  deriving (Eq, Show)

class Monoid a where
  -- | An associative operation.
  (<>) :: a -> a -> a
  -- | A unit element.
  mempty :: a

-- Coefficient-wise addition with padding; neutral is the empty list.
instance Num a => Monoid (Poly a) where
  P as <> P bs = P (addListsPad as bs)
  mempty      = P []


-- Q2. Trees & practical wrapper (TOTAL: 24 pt)
-- Elements live in leaves; internal nodes carry only a routing key (pivot).

--------------------------------------------------------------------------------
-- Provided datatype
--------------------------------------------------------------------------------

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

{- Routing invariant (BST-style):
   For any Node l k r:
     • all elements in l are strictly < k
     • all elements in r are >= k
   Elements are stored only in Leaves. The key k serves as a routing pivot.
-}

--------------------------------------------------------------------------------
-- (a) 3 pt — Membership in a BST-with-leaves
-- MARKING (a): leaf check 1, left branch 1, right branch 1.
--------------------------------------------------------------------------------

member :: Ord a => a -> Tree a -> Bool
member q (Leaf x)       = q == x
member q (Node l k r)
  | q < k               = member q l
  | otherwise           = member q r

--------------------------------------------------------------------------------
-- (b) 4 pt — Inorder traversal to ascending list
-- MARKING (b): leaf case 1, node case (linear, no ++) 3.
--------------------------------------------------------------------------------

toAscList :: Tree a -> [a]
toAscList = go []
  where
    go acc (Leaf x)     = x : acc
    go acc (Node l _ r) = go (go acc r) l  -- traverse right into acc, then left

--------------------------------------------------------------------------------
-- (c) 3 pt — Minimum (leftmost leaf)
-- MARKING (c): leaf 1, recurse-left 2.
--------------------------------------------------------------------------------

minOf :: Tree a -> a
minOf (Leaf x)     = x
minOf (Node l _ _) = minOf l

--------------------------------------------------------------------------------
-- (d) 2 pt — Join two BSTs with l < r
-- MARKING (d): correct routing key (minOf r) 2.
--------------------------------------------------------------------------------

join :: Ord a => Tree a -> Tree a -> Tree a
join l r = Node l (minOf r) r

--------------------------------------------------------------------------------
-- (e) 6 pt — Build a balanced BST bottom-up from a non-empty ascending list
-- MARKING (e): map Leaf 1, case on pairUp ts 2, root 1, recurse on level 1,
--              pairUp rule 1. Runs in O(n).
--------------------------------------------------------------------------------

buildBalanced :: [a] -> Tree a
buildBalanced = combineByLevel . map Leaf
  where
    combineByLevel :: [Tree a] -> Tree a
    combineByLevel ts = case pairUp ts of
                          []            -> error "impossible"  -- input is non-empty per spec
                          [root]        -> root
                          level@(_:_:_) -> combineByLevel level

    pairUp :: [Tree a] -> [Tree a]
    pairUp (l:r:rest) = Node l (minOf r) r : pairUp rest
    pairUp ts         = ts


--------------------------------------------------------------------------------
-- (f) 1 pt — Type of delete
-- MARKING (f): Maybe (Tree a) 1.
--------------------------------------------------------------------------------

delete :: Ord a => a -> Tree a -> Maybe (Tree a)

--------------------------------------------------------------------------------
-- (g) 5 pt — Define delete (assume helper for the right branch)
-- MARKING (g): leaf case 2, Just wrapper 1, Nothing branch 1, Just l' 1.
--------------------------------------------------------------------------------

delete x t@(Leaf y)               = if x == y then Nothing else Just t
delete x (Node l k r) | x < k     = Just (case delete x l of
                                            Nothing -> r
                                            Just l' -> Node l' k r
                                         )
                      | otherwise = implementedElsewhere x l k r

-- Provided helper for the "x >= k" branch.
implementedElsewhere :: Ord a => a -> Tree a -> a -> Tree a -> Maybe (Tree a)
implementedElsewhere x l k r = Just $
  case delete x r of
    Nothing -> l
    Just r' -> Node l k r'


-- Q3. Type inference (TOTAL: 16 pt)
-- (a) Short type spotting (4 × 1 pt = 4 pt)
-- (b) Parametric vs ad-hoc polymorphism (5 × 1 pt = 5 pt)
-- (c) Higher-order type derivation (7 pt)
--------------------------------------------------------------------------------

-- Minimal new data types (given)
data Pair a b = Pair a b
data Rose a   = R a [Rose a]
data Foo a    = A (a, Maybe a) | B (Either a (a -> a))
data Either a b = Left a | Right b


-- Solutions
e1_ty :: Pair Bool Char
e1_ty = Pair True 'x'

e2_ty :: Rose Char
e2_ty = R 'a' [R 'b' []]

-- e3_ty :: ill-typed
-- e3_ty =  A (True, Just 'x')        -- ill-typed: Bool vs Char mismatch

e4_ty :: Foo t
e4_ty = B (Right id)


--------------------------------------------------------------------------------
-- (b) Parametric vs ad-hoc polymorphism (5 × 1 pt = 5 pt)
-- Use standard Prelude types/constraints.
--------------------------------------------------------------------------------

f1 :: a -> b -> (a, b)
f1 x y       = (x, y)

f2 :: Eq a => a -> b -> Maybe b
f2 x y       = if x == x then Just y else Nothing

f3 :: (a -> b) -> a -> [b] -> [b]
f3 f x ys    = f x : ys

f4 :: (Num a, Eq b) => a -> b -> Bool
f4 n y       = const (y == y) (n + 0)

f5 :: (Num a, Eq a) => a -> Bool
f5 x         = x + 1 == x


--------------------------------------------------------------------------------
-- (c) Higher-order type derivation (7 pt)
-- Given:
--   foldl :: (b -> a -> b) -> b -> [a] -> b
--   bind  :: [a] -> (a -> [b]) -> [b]
--------------------------------------------------------------------------------

bind :: [a] -> (a -> [b]) -> [b]
bind xs f = concat (map f xs)

-- Result type after unifying constraints (final answer):
foldl_bind_ty :: [a] -> [a -> [a]] -> [a]
foldl_bind_ty = foldl bind

{-
Choosing fresh type variables to avoid clashes: -- 1 point
bind :: [a] -> (a -> [b]) -> [b]
foldl :: (d -> c -> d) -> d -> [c] -> d

It follows that -- 2 points
foldl bind :: d -> [c] -> d 
  with the constraint (d -> c -> d) ~ ([a] -> (a -> [b]) -> [b])

Simplifying the constraints -- 1 point
  d ~ [a] 
  c ~ (a -> [b])
  d ~ [b]
                    
Simplifying further -- 1 point
  d ~ [a] 
  c ~ (a -> [b])
  d ~ [b]
  [a] ~ [b]

Simplifying further -- 1 point
  d ~ [a] 
  c ~ (a -> [b])
  a ~ b

Substituting the constraints back gives  -- 1 point
foldl bind :: [a]-> [a -> [a]] -> [a]
-}


--------------------------------------------------------------------------------
-- Q4. Quick checks on lists (TOTAL: 12 pt)
-- Target list: target = [0,2,4,6,8]
--------------------------------------------------------------------------------

target :: [Int]
target = [0,2,4,6,8]

e1  = map (*2) [0..4]
e2  = [2*x | x <- [0..10], x <= 4]
e3  = [x*2 | x <- [0..10], even x]
e4  = zipWith (+) [0,2,4,6,8] [0,0,0,0,0]
e5  = zipWith (*) [0..4] [0,2..8]
e6  = take 5 (scanl (+) 0 [2,2,2,2,2])
e7  = concat (map (\x -> [2*x]) [0..4])
e8  = [0,2..8]
e9  = map snd (zip [0..4] [0,2..10])
e10 = [x+y | (x,y) <- zip [0..4] [0,2..8]]
e11 = foldr (\x acc -> 2*x : acc) [] [0..4]
e12 = foldl (\acc x -> 2*x : acc) [] [0..4] -- this was a typo on the original exam, so the question was dropped

-- Provided definitions as in the question statement
zipWith                 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys
zipWith _ []     _      = []
zipWith _ _      []     = []

-- This is a simplified implementation
scanl     :: (b -> a -> b) -> b -> [a] -> [b]
scanl f z = foldl (\acc x -> acc ++ [f (last acc) x]) [z]

-- Make the module runnable as an executable (no-op).
main :: IO ()
main = pure ()
