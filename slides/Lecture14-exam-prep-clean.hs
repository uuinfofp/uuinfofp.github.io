module Lecture14ExamPrep where 
import Prelude hiding (lookup)

--------------------------------------------------------------------------------
-- * 1. Maps and functors 
--------------------

-- We can give a simplified (and inefficient) implementation of Map-types
-- by defining.

newtype Map k v = Map [(k,v)] deriving Show

-- For example, we think of 
exampleMap = Map [('x',2), ('y', 32), ('z',42)]
exampleMap2 = Map [('x',2), ('y', 32), ('z',42),('x', 32)]
-- as arrays that associate the keys 'x', 'y' and 'z' with 2, 32, and 42 respectively.


-- In reality, Map uses a balanced binary search tree under the hood rather than a list.
-- Hence, its interface is only implemented for types k of keys that are ordered.


-- (a) Implement the interface

keys :: Ord k => Map k v -> [k]
-- for retrieving all keys
assocs :: Ord k => Map k v -> [(k,v)]
-- for retrieving all key-value associations as a list
fromList :: Ord k => [(k,v)] -> Map k v
-- for building a Map from a list of key-value associations
insert :: Ord k => k -> v -> Map k v -> Map k v
-- for inserting a new key-value association
delete :: Ord k => k -> Map k a -> Map k a
-- for deleting a key (and its associations)
lookup :: Ord k => k -> Map k a -> Maybe a
-- for looking up the (first) association of a key in the Map
empty :: Map k a
-- for the empty Map that does not associate any keys to values


keys = undefined 
assocs = undefined 
fromList = undefined 
insert = undefined 
delete = undefined 
lookup = undefined 
empty = undefined




-- (b) Give a functor instance for Map, only using the interface above.



--------------------------------------------------------------------------------
-- * 2. Induction proofs
----------------------
-- (a) Prove that foldr (:) [] = [].

-- Clearly state any induction hypotheses you need and label them I.H..
-- Please label each reasoning step either as I.H. (for induction hypothesis) or 
-- by one of the following equations 
-- [definitions foldr and id]
-- a -- foldr op e [] = e 
-- b -- foldr op e (x:xs) = op x (foldr op e xs)
-- c -- id x = x






-- (b) Prove that foldr op e = foldl op e in case op and e satisfy the monoid laws.

-- Clearly state any induction hypotheses you need and label them I.H..
-- Please label each reasoning step either as I.H. (for induction hypothesis) or 
-- by one of the following equations 
-- [definitions foldr and foldl]
-- a -- foldr op e [] = e 
-- b -- foldr op e (x:xs) = op x (foldr op e xs)
-- c -- foldl op e [] = e 
-- d -- foldl op e (x:xs) = foldl op (op e x) xs
-- [monoid laws for op and e]
-- e -- op e x  = x 
-- f -- op x e  = x 
-- g -- op x (op y z) = op (op x y) z
-- Hint: distinguish the cases of these functions applied to an empty list, a list with one element, 
-- and a list with at least two elements.








--------------------------------------------------------------------------------
-- * 3. Data types and Testing and Functors
--------------------

-- A heap is a data structure described by a data type quite similar to a search tree:
data Heap a = Top a (Heap a) (Heap a)
            | Empty deriving Show
-- with the so called (max) heap property that the a value in a Top node is larger than or equal to the
-- values in the roots of its child Heaps, which have this property themselves too. An example of a
-- heap is
aHeap = Top 10 (Top 7 Empty (Top 4 Empty Empty )) (Top 3 (Top 2 Empty Empty ) Empty )
-- Counter examples are
notAHeap1 = Top 6 (Top 7 Empty (Top 4 Empty Empty )) (Top 3 (Top 2 Empty Empty ) Empty )
notAHeap2 = Top 10 (Top 7 Empty (Top 4 Empty Empty )) (Top 1 (Top 2 Empty Empty ) Empty )

-- (a) Give a Functor instance for Heap 






-- (b) Write a function
checkHeap ::Ord a => Heap a -> Bool
-- which returns True if its argument has
-- the required propery, and False otherwise.
-- Hint: you may want to write a helper function
checkHeap0 :: Ord a => a -> Heap a -> Bool


checkHeap = undefined 
checkHeap0 = undefined





-- (c) Write a function
getFromHeap :: Ord a => Heap a -> Maybe (a, Heap a)
-- that – provided the heap is non-empty–
-- returns the largest a value from its Heap a argument, together with
-- a Heap a containing the rest of the values of its Heap a argument, and Nothing if the Heap a
-- argument is a Empty .
-- Make sure the resulting Heap a again satisfies the heap property!
-- Hint: the new root of the heap is either the root of the left subtree or the root of the right subtree.

getFromHeap = undefined





-- (d) Formulate a specification stillAHeap to test that the heap property is preserved
-- when removing an element from a heap using getFromHeap.


stillAHeap = undefined

--------------------------------------------------------------------------------
-- * 4. Laziness
--------------------
-- (a) Write down the WHNF of the following expressions

-- 1. map undefined [1,2,3,4]
-- 2. foldr (+) 0 [1,2,3]
-- 3. filter even [1,2,3,4]
-- 4. Just (2 * 2)
-- 5. (\x -> 3 + 7) undefined
-- 6. (\x y -> x * y) 42
-- 7. foldl (+) 0 
-- 8. fst ((\x -> undefined) 25, [2 * 2])
-- 9. snd ((\x -> undefined) 25, [2 * 2])
-- 10. case [1,2,3] of 
--       []    -> True 
--       _ : _ -> False
-- 11. (\x -> 3 + 7)











-- (b) The function 
-- const :: a -> b -> a 
-- const a b = a 
-- is strict/non-strict in its first argument and 
--    strict/non-strict in its second argument.

-- Write a function 
-- const' that calculates the same value as const but is strict in both its arguments.


const' = undefined



-- Consider the data type of binary trees:

data Tree a = Leaf a | Node (Tree a) (Tree a)
-- we define the function tja:

tja t = let tja' (Leaf a)   n ls = (0, if n == 0 then a : ls else ls)
            tja' (Node l r) n ls = let (lm, ll) = tja' l (n-1) rl
                                       (rm, rl) = tja' r (n-1) ls
                                    in ((lm `min` rm) + 1, ll)
            (m, r) = tja' t m []
         in r
-- (c) What is the type of tja?






-- (d) If this code computes something, explain what it computes, maybe with the aid of a small example like
exampleTree  = Node (Node (Node (Leaf 5) (Leaf 8)) (Node (Leaf 28) (Leaf 7))) (Node (Node (Leaf 5) (Leaf 8)) (Node (Leaf 28) (Node (Leaf 7) (Leaf 23))) )
-- If it does not compute anything, explain why this is the case.











--------------------------------------------------------------------------------
-- * 5. Testing and folds
--------------------

-- Consider the function
tails :: [a] -> [[a]]
tails [] = [[]]
tails (x:xs) = (x:xs) : tails xs
-- from the Data.List library,
-- which computes all the possible final segments of elements in a list.

-- (a) Reimplement tails using foldr and without using direct recursion.
tails' :: [a] -> [[a]]
tails' = undefined



-- We shall be writing QuickCheck tests to verify that your implementation is correct.

-- (b) Write a QuickCheck property that checks that the correct number of final segments is generated.

propTailsLength = undefined


-- (c) Write a function isFinal :: Eq a => [a] -> [a] -> Bool that verifies that the first argument is a 
-- final segment of the second argument

isFinal = undefined



-- (d) Write the QuickCheck property that every list in the output of tails is
-- a final segment of the input.

propEveryEltIsFinal = undefined

-- (e) Formulate a set of properties to completely characterize the tails function
-- (you may choose also from among the ones you have just implemented).
-- Make sure to remove properties that are implied by (a subset of) the other properties.
-- Implement the properties that you still need as QuickCheck properties.

propTailsCorrect = undefined



--------------------------------------------------------------------------------
-- * 6. Functors and Monads
--------------------
-- Given the type:

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show
-- of expressions built from variables of type a, show that this type is monadic by completing the following declaration:

-- (a) Write a functor instance for Expr

instance Functor Expr where 
    fmap = undefined



-- (b) Complete the monad instance for Expr below

instance Monad Expr where
    -- return :: a -> Expr a
    return = undefined

    -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (>>=) = undefined

instance Applicative Expr where 
    pure = return 
    mf <*> ma = do 
        f <- mf 
        a <- ma 
        return (f a)



-- (c) With the aid of an example, explain what the (>>=) operator for this type does.







--------------------------------------------------------------------------------
-- * 7. IO
--------------------

-- (a) Write a function of type
concatenateFiles :: [FilePath] -> FilePath -> IO ()
-- which concatenates a list of files to a specific target file:
-- the first parameter is a list of
-- filenames and the second parameter the name of the target file.
-- Do not use the function appendFile.


concatenateFiles = undefined




-- (b) Write a program that first asks for the name of the target file,
-- and then continues asking for names of files to be appended to that file
-- until an empty line is entered. Note that the target files may be one of the source files!
askContinueAsking :: IO () 

askContinueAsking = undefined




-- (c) If we know that none of the source files equals the target file we may do
-- a bit better using the function appendFile from System.IO.
-- Change the function you have written above using this function.
-- What are the advantages and disadvantages of this approach?

concatenateFiles' :: [FilePath] -> FilePath -> IO ()
concatenateFiles' = undefined






