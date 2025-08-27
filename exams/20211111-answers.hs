{- 
Topics:
- functors, monads
- IO 
- laziness
- equational reasoning and induction 
- testing
- basic programming, recursion and higher order functions
- data structures

-}
import Test.QuickCheck
import qualified Data.Set as Set

----------------------------------------------
-- 1. Equational reasoning and induction --
----------------------------------------------

-- a.

-- Some definitions that are specific to this question:
type Writer a = (String, a)
combine :: Writer a -> (a -> Writer b) -> Writer b 
combine (m, a) f = (m ++ fst (f a), snd (f a)) 

noWrite :: a -> Writer a
noWrite a = ("", a) 



-- Claim: combine (noWrite a) f = f a

-- Proof
{-
combine (noWrite a) f
= -- b
combine ("", a) f 
= -- a
("" ++ fst (f a), snd (f a))
= -- c
(fst (f a), snd (f a))
= -- i
f a
-}


-- b. 


-- Some definitions that are specific to this question:

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

flatten :: Tree a -> [a]
flatten (Leaf n) = [n] 
flatten (Node l r) = flatten l ++ flatten r 
 
flattenAcc :: Tree a -> [a] -> [a]
flattenAcc (Leaf n) ns = n : ns 
flattenAcc (Node l r) ns = flattenAcc l (flattenAcc r ns) 


-- Claim: flatten t ++ ns = flattenAcc t ns

-- We prove this by induction on t, distinguishing the cases
-- t = Leaf i and t = Node l r

-- Base case: t = Leaf i 
{-
flatten (Leaf i) ++ ns
= -- e
[i] ++ ns
= -- desugaring list syntax
(i : []) ++ ns
= -- d
i : ([] ++ ns)
= -- c
i : ns
= -- g
flattenAcc (Leaf i) ns

Inductive case: t = Node l r 
we assume the induction hypothesis that 
flatten l ++ ns = flattenAcc l ns
AND 
flatten r ++ ns = flattenAcc r ns
FOR ALL lists ns

then

flatten (Node l r) ++ ns
= f
(flatten l ++ flatten r ) ++ ns
= l
flatten l ++ (flatten r ++ ns)
= I.H.
flatten l ++ (flattenAcc r ns)
= I.H.
flattenAcc l (flattenAcc r ns)
= h
flattenAcc (Node l r) ns

Our claim follows by induction on t.

-}


----------------------------------------------
-- 2. Some basic programming and monads  --
----------------------------------------------

-- a. Write a function
lookUnsafe :: Eq k => k -> [(k, v)] -> v
-- such that lookUnsafe k kvs returns the first v such that (k, v) is in kvs.
-- lookUnsafe k kvs may crash if there is no (k, v) such that (k, v) is in kvs.
 
lookUnsafe k ((k', v) : kvs) | k == k' = v 
                             | otherwise = lookUnsafe k kvs

-- b. Consider the data type 
data Reader s a = MkReader (s -> a)
-- We think of a value of type Reader s a as an impure computation 
-- that computes a value a with access to some read-only memory 
-- of type s .

-- Define an operation
get :: Reader s s 
-- that returns the current value of the memory.

get = MkReader id


-- c. Please give a Functor instance for Reader s,
-- such that fmap f m applies a function f to the result 
-- of a computation m that reads state of type s
instance Functor (Reader s) where 
    fmap f (MkReader g) = MkReader (f.g)

-- d. We can turn Reader s into a monad.

instance Applicative (Reader s) where 
    pure = return
    (<*>) af aa = do
        f <- af 
        a <- aa 
        return (f a)

instance Monad (Reader s) where 
    return a = MkReader (const a)
    (MkReader f) >>= g = MkReader (\s -> let a = f s in let MkReader h = g a in h s)
-- The idea is that Reader s behaves like the State s monad with the only exception 
-- that we cannot write to the memory and we can only read it.


-- Some example code in this monad:
withEnv :: String -> Reader String String
withEnv s = do 
    env <- get
    return (env ++ s)

wws :: Reader String String
wws = do 
  w <- withEnv "What? " 
  w2 <- withEnv "Who? " 
  ss <- withEnv "Chika-chika Slim Shady"
  return (w ++ w2 ++ ss)

lyrics :: String
lyrics = runReader wws "My name is "
-- this is the obvious monad instance: a state monad without put.

-- Using the Reader monad, we can write an evaluator 
-- for an expression of type
data Expr =
    Val Int   -- integer value
  | Mult Expr Expr  -- multiplication
  | Var String -- variable
-- as a computation that has access to a read-only bit of 
-- memory that stores an association list m :: [(String, Int)]
-- in which we can look up the values of various variables.

-- Please write this evaluator.
-- Hint: you may want to use the function lookUnsafe from a.
-- If you prefer, you may also write 
-- eval :: Expr -> State [(String, Int)] Int 

eval :: Expr -> Reader [(String, Int)] Int 
eval (Val i) = return i 
eval (Mult e e') = do 
    v <- eval e
    v' <- eval e'
    return (v * v')
eval (Var s) = do 
    m <- get 
    return (lookUnsafe s m)



--- e.

-- Given:
runReader                 :: Reader s a -> s -> a
runReader (MkReader f) s0 = f s0

evalR :: Expr -> [(String,Int)] -> Int 
evalR = runReader . eval

readKeyValues :: String -> [(String,Int)]
readKeyValues = map read . words


-- e) implement a function 'runWithEnv' that takes an expression, and
-- allows the user to input a list of (variableName,value) pairs
-- separated by spaces, and print the value after evaluating the expression.

-- write this function *without* using do-notation

-- e.g.

--   runWithEnv (Var "v")
-- ("v",5)
-- 5
-- λ> runWithEnv (Mult (Var "v") (Mult (Val 2) (Var "x")))
-- ("v",5) ("x",2)
-- 20


runWithEnv      :: Expr -> IO ()
runWithEnv expr = getLine >>= \s ->
                    print (evalR expr (readKeyValues s))
-- OR 
--runWithEnv expr = getLine >>= \s ->
--                    print (evalR expr (readKeyValues s)) >>= return
-- if you interpreted the question as needing to use return as well





-------------------------------------
-- 3. Laziness --
-------------------------------------

-- One simple definition of sort is
sort [] = []
sort (x:xs) = insert x (sort xs) where
    insert x [] = [x]
    insert x (y:ys) = if x <= y then x:y:ys else y:insert x ys
-- This method is called insertion sort.

-- a. Reduce sort [4,3] to WHNF under lazy evaluation (only give the final result)

{-
--Reduction sequence--
sort [4,3]
insert 4 (sort [3])
insert 4 (insert 3 (sort []))
insert 4 (insert 3 [])
insert 4 [3]
if 4 <= 3 then 4:3:[] else 3 : insert 4 []
3 : insert 4 []

-- so the final result is 3 : insert 4 []
-}



-- b. Consider the definitions 
-- length :: [a] -> Int
-- length = foldl (\n x -> n+1) 0

length' :: [a] -> Int
length' = length2 0

length2 :: Int -> [a] -> Int
length2 n [] = n
length2 n (x:xs) | n==0 = length2 1 xs 
                 | otherwise = length2 (n + 1) xs

-- length and length' both compute the length of a list.

-- Describe the difference in space and time usage of length and length'.


-- length' is more efficient because it uses constant space 
-- while length uses linear space.
-- The reason that length' is constant space 
-- is because the equality comparison n==0 forces the evaluation of 
-- n in each step of the recursion.
-- Both have linear time complexity as they both need to traverse 
-- the whole list from start to tend to compute its length.


--------------------------------------------------
-- 4. Data types and testing --
--------------------------------------------------
-- Consider (again) the following type of binary trees:
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every
-- node differs by at most one, with leaves themselves being trivially balanced.

-- a. Define a property
isBalanced :: Tree a -> Bool
-- that checks whether a binary tree is balanced or not.

isBalanced (Leaf _) = True 
isBalanced (Node l r) = isBalanced l && isBalanced r && closeto (count l) (count r) where
    closeto i j = abs (i - j) <= 1
    count (Leaf _) = 1 
    count (Node l r) = count l + count r 
    -- or count = length . flatten


-- b. Define a function 
balance :: [a] -> Tree a 
-- that converts a non-empty list l into a balanced tree t
-- such that flatten t = l.
-- Hint: use the function split that splits a list into two halves whose length differs by at most one.
split :: [a] -> ([a], [a])
split xs = (take n xs, drop n xs) where 
    n = length xs `div` 2


balance [] = error "Cannot balance empty list"
balance [a] = Leaf a 
balance as = let (as1, as2) = split as in Node (balance as1) (balance as2)


-- c. Define a specification propBalanceCorrect that checks whether an implementation balance' of balance is correct
propBalanceCorrect :: Eq a => ([a] -> Tree a) -> [a] -> Bool 
propBalanceCorrect balance' xs = null xs || (isBalanced (balance' xs) && flatten (balance' xs) == xs)



-- d. Write an Arbitrary instance of Tree a that generates balanced binary trees.  
-- Hint: remember that the function balance crashes for empty lists!
instance Arbitrary a => Arbitrary (Tree a) where 
    arbitrary = do
        x <- arbitrary 
        xs <- arbitrary
        return (balance (x:xs))  

-- OR, better :

-- split' :: Int -> [a] -> ([a], [a])
-- split' n xs = (take n xs, drop n xs)

-- splits :: [a] -> [([a], [a])]
-- splits xs | even n = [split' m xs]
--           | otherwise = [split' m xs, split' (m+1) xs] where 
--               n = length xs
--               m = n `div` 2

-- balances :: [a] -> [Tree a]
-- balances [a] = [Leaf a]
-- balances as = do 
--     (as1, as2) <- splits as 
--     l <- balances as1 
--     r <- balances as2
--     return (Node l r)

-- instance Arbitrary a => Arbitrary (Tree a) where 
--     arbitrary = do 
--     x <- arbitrary
--     xs <- arbitrary
--     let ts = balances (x:xs)
--     elements ts



--------------------------------------------------
-- 5. Reachability in a Graph --
--------------------------------------------------
-- Consider the following (abstract) data type of Sets of as.
-- 1. We can form an empty set 'empty' (which does not have any elements)
-- 2. we can use the function 'insert' on an element 'a' and an existingen 
-- set 's' to obtain a new set 'insert a s', which contains all the elements that s 
-- did as well as 'a'
-- 3. we can use 'member a s' to check whether a given 'a' is an element of a set 's'.

empty :: Ord a => Set a
insert :: Ord a => a -> Set a -> Set a
member :: Ord a => a -> Set a -> Bool


-- For example, we could use the following implementation
-- type Set a = Set.Set a 
-- empty = Set.empty
-- insert = Set.insert 
-- member = Set.member

-- OR 

type Set a = [a]
empty = []
insert = (:)
member = elem


-- a. Using a fold, write a function 
fromList :: Ord a => [a] -> Set a 

fromList = foldr insert empty


-- b. Please complete the following code that traverses a graph to count the number of graph nodes reachable from the given
-- starting node. This count should include the starting node and thus will always be at least 1.


reachable :: Ord a => (a -> [a]) -> a -> Set a
reachable nexts node = reachableVis nexts node empty
reachableVis :: Ord a => (a -> [a]) -> a -> Set a -> Set a
reachableVis nexts node visited  
    | node `member` visited = visited
    | otherwise =
        foldr (reachableVis nexts) (insert node visited) (nexts node)




-- c. Does reachable use a depth-first or breadth-first traversal?
-- A. depth-first  (2pt)
-- B. breath-first (-1pt)
-- C. don't know (0pt)


testGraph :: Int -> [Int]
testGraph 1 = [2]
testGraph 2 = [4, 5]
testGraph 3 = [2]
testGraph 4 = [3]
testGraph 5 = [7]
testGraph 6 = [2, 7, 8]
testGraph 7 = []
testGraph 8 = [7]








