module Exam221006 where
-- Midterm 2022-10-06


-- TOPICS TO EXAMINE:
-- basic programming
-- recursion on lists
-- (optional) list comprehensions
-- basic data structures
-- recursion on trees
-- type classes type inference
-- (optional) more advanced data structures (e.g. associative arrays/graphs)
-- higher order functions - easy (map, filter)
-- higher order functions - harder (foldr/foldl)
-- (optional) evaluation
-- (optional) accumulators


import qualified Data.Map as Map
import qualified Data.List as List


--------------------------------------------------------------------------------
-- * 1. Points and Intervals 
--------------------------------------------------------------------------------

-- Let us define a type synonym
type Point = Float 
-- whose elements we think of as points on a line (the number line).

-- We define a datatype 
data Interval = MkInterval Point -- starting point
                           Point -- end point
              deriving (Show)
-- whose elements we think of as intervals in the number line 
-- demarked by a starting point and an endpoint. We use the convention
-- that Intervals are closed in the sense that they contain both 
-- endpoints.

-- a. [2pt] write a function
contains :: Interval -> Point -> Bool
-- that checks if an interval contains a given point
contains (MkInterval s e) q = s <= q -- 1pt
                                && q <= e -- 1pt



-- b. [3pt] Write, using direct recursion, a function
stabs :: Point -> [Interval] -> [Interval]
-- that, given a point and a list of
-- intervals returns all intervals that are "stabbed" by the point,
-- that is, all intervals that contain that point.
q `stabs` [] = [] -- 1pt
q `stabs` (i:ints) | i `contains` q = i : (q `stabs` ints) -- 1pt
                   | otherwise      = q `stabs` ints -- 1pt

-- c. [2pt] Now, rewrite "stabs" using higher order functions,
-- without using direct recursion.
stabs q ints = filter (`contains` q) ints
-- 1pt idea to use filter
-- 1pt using correct predicate






-- d. [2pt] Write, using a list comprehension, a function 
containments             :: [Interval] -- list of intervals
                         -> [Point] -- list of points
                         -> [(Point,[Interval])] -- for every point, the intervals that contain it.
-- that given a list of intervals,
-- and a list of points, returns for each point the intervals it
-- stabs.
--
--
containments ints points = [ (q, q `stabs` ints) -- 1pt
                           | q <- points -- 1pt




-- e. [3pt] Using foldr/foldl write a function
countContainments :: [(Point, [Interval])] -> Int

-- such that 
totalIntersections :: [Interval] -> [Point] -> Int 
totalIntersections ints points = countContainments $ containments ints points
-- computes the total number of intersections
-- between a list of intervals and a list of points.
countContainments = foldr f e
  where
    e = 0 -- 1pt
    f (_, is) a = length is + a -- 2pt




--------------------------------------------------------------------------------
-- * 2. Type inference
--------------------------------------------------------------------------------

-- Please determine the types of the following expressions
-- or show that they are ill-typed.
-- Please write down all reasoning steps, as they are at least as
-- important as the final answers.



-- a. [5pt] flip foldr
{-
Assumption:
foldr :: (a -> b -> b) -> b -> ([a] -> b)
flip :: (c -> d -> e) -> (d -> c -> e)
-- (Use fresh type identifiers -- 1pt)


Therefore,
flip foldr :: d -> c -> e -- 1pt
with the constraint that (a -> b -> b) -> b -> ([a] -> b) ~ c -> d -> e -- 1pt

Resolving the constraints gives us that
a -> b -> b ~ c 
b ~ d 
[a] -> b ~ e .
-- 1pt

Therefore, 
flip foldr :: b -> (a -> b -> b) -> ([a] -> b)
-- 1pt


-}


-- b. [6pt] (.). map

{-


Assumption:
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) :: (e -> f) -> ((d -> e) -> (d -> f))
map :: (g -> h) -> ([g] -> [h])
(Note fresh type variables) -- 1pt


Therefore (.) . map :: a -> c with the constraints that  -- 1pt
(b -> c) ~ (e -> f) -> ((d -> e) -> (d -> f))
(a -> b) ~ (g -> h) -> ([g] -> [h])  -- 1pt

Resolving the constraints gives us that 
b ~ e -> f 
c ~ (d -> e) -> (d -> f)
a ~ g -> h 
b ~ [g] -> [h] -- 1pt

Resolving further, we find that 

e ~ [g]
f ~ [h] -- 1pt



Therefore,

(.) . map :: (g -> h) -> (d -> [g]) -> (d -> [h]) -- 1pt
-}





--------------------------------------------------------------------------------
-- * 3. Trees
--------------------------------------------------------------------------------
-- Consider the following data type modeling binary trees (that store
-- elements in both the leaves and the internal nodes.)

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Show)


-- a. [4pt] Complete the following definition
longestPath :: Tree a -> [a]
longestPath = snd . longestPath'

longestPath'              :: Tree a -> (Int,[a])
-- to make longestPath compute the longest root to leaf path in the tree.
longestPath' (Leaf x)     = (1,[x]) -- 1pt
longestPath' (Node l x r) = let ((ll,lp), (rl,rp)) = (longestPath' l , longestPath' r)-- 1pt
                            in if ll < rl -- 1pt
                               then (1+rl, x:rp) else (1+ll, x:lp) -- 1pt 



-- b. [1pt] Please modify the 'Tree a' data type to let us store/read out
-- the length of the longest root-to leaf path of a subtree in the data type.
data MyTree a = MyLeaf a
              | MyNode (MyTree a) a Int (MyTree a) -- 1pt



--------------------------------------------------------------------------------
-- * 4. Circular lists
--------------------------------------------------------------------------------


-- Consider the following type modelling non-empty circular lists.

data CircularList a = MkCircularList [a]    -- consecutive subset of the elements left of the focus
                                          -- in CCW (counter clockwise) order
                                   a      -- current focus
                                   [a]    -- consecutive subset of the elements right of the focus
                                          -- in CW (clockwise) order
                                   deriving (Show)


-- Observe that the same circular list can have multiple different representations, 
-- depending on where we "cut the circle".
-- For example, 
clock5 = MkCircularList [4, 3, 2, 1] 5 [6, 7, 8, 9, 10, 11, 12]
clock5' = MkCircularList [] 5 [6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4] 
clock5'' = MkCircularList [4,3,2,1,12,11,10,9,8,7,6] 5 []
-- are three different representations of a clock that reads five o'clock
-- (5 is the focus) and
clock6 = MkCircularList [5, 4, 3, 2, 1] 6 [7, 8, 9, 10, 11, 12]
clock6' = MkCircularList [] 6 [7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5] 
-- are two different representations of a clock that reads six o'clock
-- (6 is the focus), which again are equivalent to the clocks that 
-- read 5.

triangle = MkCircularList [1] 2 [3]
notAClock = MkCircularList [4, 3, 2, 1] 5 [7, 6, 8, 9, 10, 11, 12]
-- These are not equivalent to any of the clocks as they cannot
-- be rotated into a clock!

-- a. [2pt] Write a function
cwElements :: CircularList a -> [a]
-- that returns all elements in CW
-- order, starting from the focus.
-- So, for example,
-- cwElements clock5 = [5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4]
-- cwElements clock5' = [5, 6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4].

cwElements (MkCircularList ls x rs) = x:rs ++ reverse ls
-- 1pt -- add x to rs
-- 1pt -- concatenate to reverse ls


-- b. [3pt] Write a function
goCw                          :: CircularList a -> CircularList a
-- that moves the focus in clockwise direction.
-- So, for example, 
-- goCw clock5 = MkCircularList [5, 4, 3, 2, 1] 6 [7, 8, 9, 10, 11, 12]
-- goCw clock5' = MkCircularList [5] 6 [7, 8, 9, 10, 11, 12, 1, 2, 3, 4]

goCw c@(MkCircularList ls x rs) = case rs of
                                     [] -> case reverse ls of
                                             []      -> c -- 1pt (x is the only element)
                                             (y:ls') -> MkCircularList [x] y ls' -- 1pt
                                     (r:rs') -> MkCircularList (x:ls) r rs' -- 1pt







-- c. [3pt] Write a function
isShiftOf :: Eq a => CircularList a -> CircularList a -> Bool
-- to test if the first circular list is a shift of the second.
-- You may assume that you have a function 
allRotations   :: CircularList a -> [CircularList a]
-- that computes all different shifts/rotations of a circular list.
-- Don't worry about efficiency.
cl `isShiftOf` cl' = clElts `elem` clEltss' where -- 1pt  
  clElts = cwElements cl -- 1pt
  clEltss' = map cwElements $ allRotations cl' -- 1pt

-- d. [2pt] Please make CircularList an instance of the Eq typeclass that tests if
-- two CircularLists are the same up to rotations
instance Eq a => Eq (CircularList a) where -- 1pt
  (==) = isShiftOf -- 1pt


-- e. [2pt] (Hard) Please complete the following definition of the allRotations 
-- that you used in subquestion c. above
allRotations c = take (size c) $ cwRotations c
size                      :: CircularList a -> Int
size (MkCircularList l x r) = length l + 1 + length r
-- by defining an appropriate function
cwRotations :: CircularList a -> [CircularList a]
cwRotations = iterate goCw
-- OR
cwRotations' cl = scanl (\acc _ -> goCw acc) cl [1..]
-- OR
cwRotations'' cl = cl : map goCw (cwRotations'' cl)




