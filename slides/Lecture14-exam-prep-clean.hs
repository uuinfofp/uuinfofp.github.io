module Lecture14ExamPrep where 
import Prelude hiding (lookup)
import Control.Monad.State.Lazy

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
-- Here, fmap f is intended to apply the function f to all associations in the map
-- and to leave the keys the same.


--------------------------------------------------------------------------------
-- * 2. Induction proofs
----------------------
-- (a) Prove that foldr (:) [] = id

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






--------------------------------------------------------------------------------
-- * 8. State monad
--------------------

-- In this question, we will be writing some code using the state monad.
-- Recall that State s is a monad that models computation with access 
-- to mutable state of type s.

-- We can access this state with the following operations:

-- get :: State s s
-- where get reads the current value of the state

-- put :: s -> State s ()
-- where put s overwrites the state with s

-- modify :: (s -> s) -> State s ()
-- where modify f reads the current value s of the state and overwrites it with f s

-- state :: (s -> (a, s)) -> State s a
-- where state f transforms the state according to snd . f and computes
-- the return value according to fst . f


-- We further have a (handler) function 
-- runState :: State s a -> s -> (a, s)
-- where runState x s returns the the pair (a, s') where a is the return value computed by x
-- starting from state s and s' is the updated state that x computes from s.

-- Note that state . runState = id and runState . state = id 

-- We can define a useful helper function
run :: State s a -> s -> a
run x = fst . runState x
-- where run x s only computes the return value of a stateful command x starting from 
-- initial state x, forgetting about the final value of the state.



-- Consider the following type of binary trees.
data BTree a = BLeaf | BNode (BTree a) a (BTree a) deriving (Show, Eq)
-- For example, we have the following inhabitant of this data type:
exampleBTree :: BTree String
exampleBTree = BNode (BNode (BNode BLeaf "x" BLeaf) "w" (BNode BLeaf "y" BLeaf)) "x"
                     (BNode (BNode (BNode BLeaf "x" BLeaf) "y" BLeaf) "z" (BNode BLeaf "y" BLeaf))


-- (a) Write a function
getFresh :: String -> [String] -> String
-- such that getFresh x xs checks whether the string x is an element of xs:
-- if so, it returns x; if not, it adds ' characters after x, until it is true that
-- this new 'ed x is not an element of xs; once that is true, it returns the new 'ed x.
-- We think of x as a name and say that getFresh x xs "replaces x with a
-- name that it fresh with respect to xs"
getFresh = undefined

-- (b) Using getFresh, write a function
freshNodesS :: BTree String -> State [String] (BTree String)
-- using the State monad, such that 
freshNodes :: BTree String -> BTree String 
freshNodes bt = run (freshNodesS bt) [] 
-- traverses a tree storing Strings (think of them as names) in each BNode 
-- using a depth-first, in-order traversal (i.e. visiting its left subtree before the node itself
-- before the right subtree) and during the traversal does the following things:
-- 1. keep track of a list xs of all the Strings (names) it has seen as the state
-- 2. at each BNode, it uses getFresh to replace the stored name x by one that is fresh with
-- respect to the names xs it has seen so far 
-- 3. it otherwise keeps the structure of the tree the same.


freshNodesS = undefined


-- (c) Next, we will consider a stateful program that describes the operation 
-- of a turnstile.

-- The turnstile has two states: Locked and Unlocked.
-- (It starts in a Locked state).
-- There are two types of regular input:
-- Coin (corresponding to someone putting a coin in the slot) and
-- Push (corresponding to someone pushing the arm).
-- Each input causes an output (Thank, Open or Tut) and
-- a transition to a (new, or maybe the same) state.
-- If someone puts a coin in when the turnstile is locked,
-- they are thanked (yes, it can talk!) and the turnstile
-- becomes unlocked. If they add more coins, they are thanked
-- more but get no benefit (the turnstile simply remains unlocked
-- with no memory of how many additional coins have been added).
-- If someone pushes the arm when the turnstile is unlocked, 
-- the arm will open to let them through, then become locked
-- to prevent anyone else going through.
-- If someone pushes the arm when the turnstile is locked,
-- it will politely tut at them but not let them through and remain locked.
-- The operator of the turnstile can also use a key to switch the state 
-- from locked to unlocked and back. This will not cause the turnstile to 
-- emit any output.

-- We model the states and outputs of the turnstile as follows.
data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Show, Eq)


-- We can then describe the actions as follows.
coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin Locked = (Thank, Unlocked)
coin Unlocked = (Thank, Unlocked)
push Locked = (Tut, Locked)
push Unlocked = (Open, Locked)

key :: TurnstileState -> TurnstileState 
key Locked = Unlocked 
key Unlocked = Locked

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = state coin
pushS = state push

keyS :: State TurnstileState () 
keyS = modify key

-- Consider the following bit of code in the State monad.
-- It describes what happens on a given Monday:
-- someone inserts a coin, they push the turnstile,
-- next someone else pushes the turnstile,
-- the operator uses their key,
-- the third customer arrives, inserts a coin and pushes 
-- the turnstile.
mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  keyS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]

-- Please desugar the code of mondayS from do-notation to an implementation 
-- that uses >>= explicitly instead. Call the resulting reimplementation 
-- mondayS'.

mondayS' = undefined