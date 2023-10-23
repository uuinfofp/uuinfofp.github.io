import Prelude hiding (elem,elems,takeWhile)

--------------------------------------------------------------------------------
-- * Recursion on lists and other data types

-- a) Implement
takeWhile :: (a -> Bool) -> [a] -> [a]
-- using explicit recursion

takeWhile _ [] = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

-- b) Write
elem :: Eq a => a -> [a] -> Bool
--  using foldr
-- .. i.e. complete

elem y = foldr f e
  where
    e = False
    f x acc = acc || x == y

--------------------------------------------------------------------------------
-- * Infer and check the type of an expression

{-
1) What is the type of "foldr filter". Give the derrivation.
-}

{-
Observe that
foldr :: (a -> b -> b) -> b -> [a] -> b and filter :: (c -> Bool) -> [c] -> [c] (1pt)
(note that fresh type identifiers) (1pt)

Therefore,
foldr filter :: b -> [a] -> b (1pt)
with the constraint that 
(a -> b -> b) ~ (c -> Bool) -> [c] -> [c]  (1pt)
Resolving the constraints gives us that 
a ~ c -> Bool (1pt)
b ~ [c] (1pt)
b ~ [c]

Consequently, 
foldr filter :: [c] -> [c -> Bool] -> [c] (2pt)

-}

{-
2) What is the type of "foldl filter". Give the derrivation.
-}

{-
Observe that
foldl :: (b -> a -> b) -> b -> [a] -> b and filter :: (c -> Bool) -> [c] -> [c] (1pt)
(note that fresh type identifiers) (1pt)

Therefore,
foldr filter :: b -> [a] -> b
with the constraint that 
(a -> b -> b) ~ (c -> Bool) -> [c] -> [c]  (1pt)
Resolving the constraints gives us that 
b ~ c -> Bool (1pt)
a ~ [c] (1pt)
b ~ [c] (1pt)

Resolving further, by transitivity of ~, we find that 
[c] ~ c -> Bool 

This is a contradiction, so foldl filter does not type check.

-}

--------------------------------------------------------------------------------
-- * Quad trees

-- goals:
-- define User-defined data types,
-- Define functions by pattern matching
-- Write classes and instances


-- Let us define the following two types tha tallow us to model (axis-aligned) squars:

type Point = (Int,Int)

data Square = Square { lowerLeft :: Point
                     , width     :: Int
                     }
              deriving (Show,Eq)

-- a) write a function inSquare that tests if a point lies in (or on the boundary of) a
-- square:

(x,y) `inSquare` (Square (lx,ly) w) = and [ lx <= x, x <= lx + w
                                          , ly <= y, y <= ly + w
                                          ]

-- b) write a function intersects, that tests if two (axis-aligned) squares intersect.
--
-- hint: two (axis-aligned) squares intersect if and only if one square has a corner
-- inside the other square.
intersects :: Square -> Square -> Bool
sl `intersects` sr = any (`inSquare` sr) (corners sl) || any (`inSquare` sl) (corners sr)

corners                      :: Square -> [Point]
corners (Square l@(lx,ly) w) = [l, (lx+w,ly), (lx,ly+w), (lx+w,ly+w)]

-- c) consider the following typeclass

class Eq t => PartialOrd t where
  -- returns true <=> x appears before in the partial order
  isLeq :: t -> t -> Bool

-- and for example the following instance for intervals (modeled by pairs)
instance (Ord a) => PartialOrd (a,a) where
  isLeq (l,r) (l',r') = l' <= l && r <= r'

-- make Square an instance of PartialOrd so that s `isLeq` s' if s is a subset of s'.

instance PartialOrd Square where
  isLeq s@(Square l@(lx,ly) w) s' =
    l `inSquare` s' && (lx + w, ly + w) `inSquare` s'


-- consider the following QuadTree type, which represents a recursive partition of a
-- square into four equal size squares:

data QuadTree = EmptyLeaf
              | FullLeaf Point
              | Node { region    :: Square
                     , northWest :: QuadTree
                     , northEast :: QuadTree
                     , southEast :: QuadTree
                     , southWest :: QuadTree
                     } deriving (Show,Eq)

-- myQuadTree :: QuadTree
-- myQuadTree = Node blackSquare nw ne sw se
--   where
--     nw = Node blueNWSquare EmptyLeaf EmptyLeaf (Node ...) EmptyLeaf
--     ne = ...
--     sw = ...
--     se = FullLeaf p


-- c) write a function buildQuadTree, which takes a Square and a list of points, and
-- builds a quadtree. In particular, we keep refining a square (node) until there is at
-- most one point in the square left.

buildQuadTree                          :: Square -> [Point] -> QuadTree
buildQuadTree _                    []  = EmptyLeaf
buildQuadTree _                    [p] = FullLeaf p
buildQuadTree s@(Square l@(x,y) w) pts = Node s (recurse nw) (recurse ne) (recurse se) (recurse sw)
  where
    w' = w `div` 2
--  square' :: Point -> Square
    square' p = Square p w'
--  recurse :: Square -> QuadTree 
    recurse s' = buildQuadTree s' (points s')
    points s' = [ p | p <- pts, p `inSquare` s']
--  nw, ne, se, sw :: Square
    nw = square' (x,y+w')
    ne = square' (x+w',y+w')
    se = square' (x+w',y)
    sw = square' l


-- d) quadTrees are somewhat useful to efficiently find points in some query region, write
-- a function report, that takes an arbitrary query square q, and a quadtree, and reports
-- all the points in the quadtree. Make sure to visit a the children of a node only when
-- it intersects the query square.

report               :: Square -> QuadTree -> [Point]
report _ EmptyLeaf   = []
report s (FullLeaf p)
    | p `inSquare` s = [p]
    | otherwise      = []
report s (Node t nw ne se sw)
    | s `intersects` t = concatMap (report s) [nw,ne,se,sw]
    | otherwise        = []

--------------------------------------------------------------------------------
-- * Difference lists (Define and use higher-order functions)

-- concatenation on lists is somewhat slow, so we can use the following type which allows
-- us to build strings a bit faster:
type ListBuilder a = [a] -> [a]

-- These list-builders are sometimes also known as difference lists.  We can use this type
-- to make computations with lists of type `[a]` more efficient. The idea is that the
-- input list given to the `ListBuilder a` is the remaining suffix (that we currently may
-- not know yet) that we combine with the part of the list that we did already receive.

-- We can turn any list into a ListBuilder
--
asBuilder    :: [a] -> ListBuilder a
asBuilder xs = \suffix -> xs ++ suffix

-- We tend to do this to convert the input (a list) we care about into a list-builder.
-- Next, we perform our particular computation using the list-builder representation.
-- This often allows us to easily write more efficient code than if we were working
-- directly with lists.  Once we are done, we materialize the result to an actual list by
-- running the builder on the empty list as input:

materialize         :: ListBuilder a -> [a]
materialize builder = builder []


--- a) write a function (+++) that implements the `(++)` function on list-builders. That
--- is, such that

-- materialize (asBuilder xs +++ asBuilder ys)
-- equals
-- xs ++ ys

(+++)     :: ListBuilder a -> ListBuilder a -> ListBuilder a
(+++) f g = \suffix -> (f . g) suffix


-- b) write a function concat' that performs the equivalent of the `concat` function on
-- list-builders.

concat' :: [ListBuilder a] -> ListBuilder a
concat' = foldr (+++) id

--------------------------------------------------------------------------------
-- * Write functions using accumulators

-- Consider the following function toList, that converts a tree into a list:

elems :: Tree a -> [a]
elems BLeaf = []
elems (BNode l x r) = elems l ++ [x] ++ elems r

-- where

data Tree a = BLeaf
            | BNode (Tree a) a (Tree a)
            deriving (Show)

-- this function has the same issue as we saw with the initial implementation of reverse:
-- it runs in O(n^2) time, since we repeatedly concatenate lists. Write a function
-- 'fastElems' that avoids this issue, and runs in linear time instead.
--
fastElems :: Tree a -> [a]
fastElems = go []
  where
    go acc BLeaf         = acc
    go acc (BNode l x r) = go (x : go acc r) l

-- Alternatively, we can reuse the Listbuilders from the previous question.

elems'' = materialize . elems'
  where
    elems' :: Tree a -> ListBuilder a
    elems' BLeaf = id
    elems' (BNode l x r) = elems' l . (x:) . elems' r
