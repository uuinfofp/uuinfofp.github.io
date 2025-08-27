module Exam191001 where

import           Prelude                 hiding ( any
                                                , maximum
                                                )

--------------------------------------------------------------------------------
-- * 1. Merge sort.

--------------------
-- a)
-- define 'merge' using direct recursion:

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys -- 1
merge xs [] = xs -- 1
merge l@(x : xs) r@(y : ys) | x <= y    = x : merge xs r -- 1
                            | otherwise = y : merge l ys -- 1

--------------------
-- b)
-- halve using take and drop

halve :: [a] -> ([a], [a])
halve xs = let h = length xs `div` 2 in
             (take h xs, drop h xs)

-- correct -- 3
-- correct halve for mergesort, but not spec -- 2
-- (take, drop) structure correct but not correct halve for mergesort -- 1
-- 0

--------------------
-- c)
-- mergeSort
mergeSort :: Ord a => [a] -> [a]
mergeSort []    = [] -- 1
mergeSort l@[x] = l -- 2
mergeSort xs    = merge (mergeSort ls) (mergeSort rs) -- 2
  where
    (ls, rs) = halve xs -- 1


--------------------
-- d)

-- given the following data type:

data Pair a b = MkPair a b

-- define functions `getA` and `getB` that, respectively, get the 'a' and 'b' out of a Pair a b

getA :: Pair a b -> a
getA (MkPair x _) = x -- 1

getB :: Pair a b -> b
getB (MkPair _ y) = y -- 1

--------------------
-- e)
-- define an Eq instance for (Pair a b) in which two pairs are
-- considered equal if and only if they have the same 'b' values.

instance Eq b => Eq (Pair a b) where
  MkPair _ b == MkPair _ b' = b == b'

-- define an Ord instance for (Pair a b) in which the pairs are
-- compared only on their b values.
instance Ord b => Ord (Pair a b) where
  z <= z' = getB z <= getB z'

  -- write down semantically decent instance of type class --1
  -- note that type class instance needs to be conditional --1

--------------------
-- f)
-- define a function sortOn that sorts a list of [a] values using the
-- function f. Write this using the function 'mergeSort', higher order
-- functions, and the Pair data type you defined above.

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = map getA . mergeSort . map (\x -> MkPair x (f x))
-- realize you need to create a pair (x, f x) -- 3
-- sort it -- 1
-- get the A out again -- 1


--------------------------------------------------------------------------------
-- * 2. Folds. Higher order functions, foldr, accumulator

--------------------
-- a)
-- define any using a fold
any :: (a -> Bool) -> [a] -> Bool
any p = foldr f x0 where
  f = \x r -> p x || r -- 3
  x0 = False -- 1

--------------------
-- b)
-- partition using direct recursion
partition1 :: (a -> Bool) -> [a] -> ([a], [a])
partition1 p [] = ([], []) -- 1
partition1 p (x : xs) | p x       = (x : trues, falses)
                      | otherwise = (trues, x : falses)
  where (trues, falses) = partition1 p xs -- 1

--------------------
-- c)
-- partition using a fold
partition2 :: (a -> Bool) -> [a] -> ([a], [a])
partition2 p = foldr
  (\x (trues, falses) ->
    if p x then (x : trues, falses) else (trues, x : falses)
  ) -- 4
  ([], []) -- 1

--------------------
-- d)
-- maximum' using an accumulator
-- write using an accumulator
maximum' :: Ord a => a -> [a] -> a
maximum' a []       = a
maximum' a (x : xs) = maximum' (a `max` x) xs -- 4

--------------------------------------------------------------------------------
-- * 3. Trees

-- Given:
data WTree a = Leaf Int
            | Node (WTree a) a (WTree a)


--------------------
-- a)
-- calculate the height of the tree
height (Leaf _    ) = 0 -- 1
height (Node l _ r) = 1 + height l `max` height r -- 2

--------------------
-- b)
-- calculate the sum of the integer values in leaves of the tree
totalWeight :: WTree a -> Int
totalWeight (Leaf i    ) = i -- 2
totalWeight (Node l _ r) = totalWeight l + totalWeight r -- 2


--------------------
-- c)

-- check if a tree is weight-balanced, that is, whether for each subtree the totalWeight of the left branch is at most three times  the totalWeight of the right branch and vice versa and we define leaves to always be weight-balanced, using the totalWeight function
isBalanced :: WTree a -> Bool
isBalanced (Leaf _    ) = True -- 1
isBalanced (Node l _ r) = isBalanced l && isBalanced r -- 1
                       && withinFactor3 (totalWeight l) (totalWeight r) -- 1 voor correctheid, 1 voor stijl
                       where
                        withinFactor3 x y = x `div` y <= 3 && y `div` x <= 3

--------------------
-- d)
-- now define a function which computes the totalWeight and isBalanced simultaneously
weightAndBalanced :: WTree a -> (Int, Bool)
weightAndBalanced (Leaf i) = (i, True) -- 1
weightAndBalanced (Node l _ r) =
  (lWeight + rWeight, lBalanced && rBalanced && withinFactor3 lWeight rWeight) where -- 3 voor correctheid, 1 voor stijl
  (lWeight, lBalanced) = weightAndBalanced l
  (rWeight, rBalanced) = weightAndBalanced r
  withinFactor3 x y = x `div` y <= 3 && y `div` x <= 3


--------------------
-- e)
-- now define a variation of WTrees which are polymorphic in the type of weight.
data WTreeP a b = LeafP b -- 1
                | NodeP (WTreeP a b) a (WTreeP a b) -- 2

--------------------------------------------------------------------------------
-- * 4. Type inference.

--------------------
-- a)
-- Compute the type of the following expressions or demonstrate that they are not well-typed.
-- In both cases give a formal derivation.


-------- *
-- map elem [1, 2] [1, 2, 3, 4] -- 3 voor derivation, 2 voor antwoord


-- map :: (a -> b) -> [a] -> [b]
-- elem :: Eq c => c -> [c] -> Bool
--                                      -- 1pt
--
-- dropping the Eq c => in the remainder for simplicity
--
-- so
-- a -> b ~ c -> [c] -> Bool
--
-- and thus:
--
-- a      ~ c
-- b      ~ [c] -> Bool
--
-- using that ['a','b'] :: [Char] we get
-- a ~ Char ~ c
-- b ~ [Char] -> Bool
--
--                                      -- 1pt
--
-- so:
-- map elem           :: Eq c => [c] -> [[c] -> Bool]
-- map elem ['a','b'] :: [[Char] -> Bool]
--
--                                      -- 1pt
--
-- we cannot apply a value of type [d], so in particular a list of
-- type [[Char] -> Bool] to a list. So
--
-- map elem ['a','b'] ['a','b','c','d'] is not well typed.
--                                      -- 2pt

-------- *
--
-- foldr (.)                    -- 3 voor derivation, 2 voor antwoord


-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- (.)   :: (d -> e) -> (c -> d) -> c -> e
--                                      -- 1pt
--
--
-- a ~ d -> e
-- b ~ c -> d
-- b ~ c -> e
--                                      -- 1pt
--
-- so:
--
-- d ~ e
--
-- so:
-- a ~ d -> d
-- b ~ c -> d
--                                      -- 1pt
--
-- thus:
--
-- foldr (.) :: (c -> d) -> [d -> d] -> (c -> d)
--
--                                      -- 2pt
--
-- (or if you want to rename this to 'a's and 'b's):
--
-- foldr (.) :: (a -> b) -> [b -> b] -> a -> b

--------------------
-- b)
-- Which of the following statements are true:

-- 1) map (:) has type [b] -> [[b] -> [a]] -- -4
-- 2) (== 4) :: (Eq a, Num a) => a -> Bool -- -4

--answer: only 2

-- -4 per incorrect answer

--------------------------------------------------------------------------------
-- * 5. Multiple choice

--------------------
-- a)

-- Circle all of the expressions that have the same value as
f = \x -> [x, x]
p = (> 'd')
q = (< 'p')
xs = "alskgjlkaingaslkn"
target = [ (f x, x) | x <- xs, p x, q x ]

-- a)     --- DOESN'T TYPE CHECK
--optionA = (filter p . filter q . map (\x -> (f x, x))) xs -- -3
-- b)     --- DOESN'T TYPE CHECK
--optionB = filter (\y -> p y && q y) (map f xs) -- -3
-- c)     --- WRONG TYPE
optionC = map (\x f -> (x, f x)) (filter p (filter q xs)) -- -2
-- d)
optionD = (map (\x -> (f x, x)) . filter p . filter q) xs -- -2

-- answer: d

-- -2 per incorrect answer

--------------------
-- b)

-- given the data type
data Zoo = MkZoo [(Enclosure, [Animal])] [Staff]
data Enclosure = PlotOfSand
               | Jungle
               | Aquarium deriving Eq
data Animal = Gorilla
             | Hippo
             | Crab
             | GoldFish
             | Other String deriving Show
data Staff = MkStaff String Int

-- which of the following expressions are of well-typed of type Zoo?
-- a = MkZoo [(Aquarium, GoldFish), (Jungle, Gorilla)]
--           [MkStaff "Frank" 31, MkStaff "Matthijs" 29] -- -3

b = MkZoo
  [(PlotOfSand, []), (Aquarium, [Hippo, Crab, Gorilla, Crab, Other "Rat"])]
  [MkStaff "Henk" 52, MkStaff "Ingrid" 42] -- -3

-- cZoo = MkZoo [(Aquarium, [Other "Shark", GoldFish "Bertus"])] [] -- -3

-- answer: b

-- -3 per incorrect answer

--------------------
-- c)

-- Clearly, some enclosures are not suitable for some animals. For example, animals that cannot swim cannot be held in an aquarium.
-- Which of the following implementations of `drowns` checks correctly which animals are at risk?

drowns1, drowns2, drowns3 :: (Animal -> Bool) -> Zoo -> [Animal]
drowns1 canSwim (MkZoo eas _) =
  [ a | as <- aquariumAnimals, a <- as, not (canSwim a) ]
 where
  aquariumAnimals =
    map (\(_, as) -> as) (filter (\(e, _) -> e == Aquarium) eas) -- -3

drowns2 canSwim (MkZoo eas _) =
  concat [ filter (not . canSwim) as | (Aquarium, as) <- eas ] -- -3

drowns3 canSwim (MkZoo (ea : eas) s) = getDrowningAnimals ea
  ++ drowns3 canSwim (MkZoo eas s) where
  getDrowningAnimals (Aquarium, a : as) = filter (not . canSwim) as  -- -3

-- answer: drowns1 and drowns2

-- -3 per incorrect answer
