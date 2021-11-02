import Test.QuickCheck
import System.Random
import System.Directory

--------------------------------
-- Equational reasoning, induction
--------------------------------
-- We first ask you to prove some equations.
-- In your proofs, please motivate every reasoning step with 
-- either a definition, a law, or the use of an induction hypothesis.

-- a.
-- Some useful definitions:
data Tree a = Leaf | Node (Tree a) a (Tree a)

size :: Tree a -> Int
size Leaf = 0 
size (Node l x r) = (size l + 1) + size r

enumInfix :: Tree a -> [a]
enumInfix Leaf = []
enumInfix (Node l x r) = (enumInfix l ++ [x]) ++ enumInfix r

length :: [a] -> Int
length [] = 0 
length (x:xs) = 1 + length xs

(++) :: [a] -> [a] -> [a] 
[] ++ xs = xs 
(y:ys) ++ xs = y : ys ++ xs


-- And some useful laws:
length . map f    = length
length (xs ++ ys) = length xs + length ys
length . concat   = sum . map length
sum (xs ++ ys) = sum xs + sum ys
sum . concat   = sum . map sum
map f . concat = concat . map (map f) 
n + 0 = n 
0 + n = n 
(k + l) + m = k + (l + m)


-- Using these definitions and laws, prove the following claim:
size t = length (enumInfix t)


-- Proof:
-- We prove this by induction on t, distinguishing the cases 
-- t = Leaf and t = Node l x r 

-- Base case: t = Leaf 
size Leaf
= -- def1 size
0
= -- def1 length
length []
= -- def1 enumInfix
length (enumInfix Leaf)


-- Inductive case: t = Node l x r 
-- We assume the induction hypotheses 
size l = length (enumInfix l) -- I.H.1 
size r = length (enumInfix r) -- I.H.2

size (Node l x r)
= -- def2 size 
(size l + 1) + size r
= -- I.H.1
(length (enumInfix l) + 1) + size r
= -- I.H.2
(length (enumInfix l) + 1) + length (enumInfix r)
= -- lunit +
(length (enumInfix l) + (1 + 0)) + length (enumInfix r)
= -- def1 length
(length (enumInfix l) + (1 + length [])) + length (enumInfix r)
= -- def2 length
(length (enumInfix l) + length [x]) + length (enumInfix r)
= -- law about length
length (enumInfix l ++ [x]) + length (enumInfix r)
= -- law about length
length ((enumInfix l ++ [x]) ++ enumInfix r)
= -- def2 enumInfix
length (enumInfix (Node l x r))

-- The claim now follows by induction on t.








-- b. Show that the Maybe monad satisfies the monad laws.

return x = Just x 
Nothing >>= _ = Nothing 
(Just x) >>= f = f x

-- Claim:
(return x) >>= f = f x 

-- Proof:
(return x) >>= f 
= -- def return 
(Just x) >>= f 
= -- def2 >>=
f x



-- Claim:
mx >>= return = mx 

-- Proof: 
-- Do a case distinction on mx: mx=Nothing, mx=Just x

-- mx=Nothing
Nothing >>= return
= -- def1 >>=
Nothing 


-- mx=Just x
(Just x) >>= return
= -- def2 >>=
return x 
= -- def return 
Just x 



-- Claim: 
(mx >>= f) >>= g = mx >>= (\x -> f x >>= g)

-- Proof:
-- Do a case distinction on mx: mx=Nothing, mx=Just x'


-- mx=Nothing
(Nothing >>= f) >>= g 
= -- def1 >>=
Nothing >>= g 
= -- def1 >>=
Nothing 
= -- def1 >>=
Nothing >>= (\x -> f x >>= g)

-- mx=Just x'
(Just x' >>= f) >>= g 
= --def2 >>= 
f x' >>= g 
= -- beta-reduce
(\x -> f x >>= g) x'
= -- def2 >>=
Just x' >>= (\x -> f x >>= g)




--------------------------------
-- Functors
--------------------------------

-- Consider the data type
data Deque a = Empty
    | Single a
    | Multiple (Access a) (Deque (a,a)) (Access a)
        deriving (Show,Eq)

data Access a = One a | Two a a deriving (Show,Eq)

-- Make Deque an instance of Functor



instance Functor Access where 
    fmap f (One a) = One (f a)
    fmap f (Two a a') = Two (f a) (f a')

instance Functor Deque where 
    fmap f Empty = Empty
    fmap f (Single a) = Single (f a)
    fmap f (Multiple aa1 daa aa2) = Multiple (fmap f aa1) (fmap (\(x,y) -> (f x, f y)) daa) (fmap f aa2)


--------------------------------
-- Monads
--------------------------------
-- Consider the data type
data Reader s a = MkReader (s -> a)

-- a. Make Reader s an instance of Monad

instance Monad (Reader s) where 
    return a = MkReader (const a)
    (MkReader f) >>= g = MkReader $ \s -> let a = f s in let MkReader g' = g a in g' s

instance Applicative (Reader s) where 
    mf <*> ma = do 
        f <- mf 
        a <- ma 
        return (f a)

instance Functor (Reader s) where 
    fmap f ma = do 
        a <- ma 
        return (f a)


-- Consider the following piece of code, where
g :: Int -> Maybe Int 
g = undefined
h :: a -> Maybe a
h = undefined
f w = do x <- g w
         let xs = do z <- [1, 2]
                     v <- ['a', 'b']
                     return (z, v)
         y <- h (snd (head xs))
         return y

{-
b. Complete the following sentence by filling in the gaps:
In the Maybe monad, (1) signals failure and (2) a successful computation. In the above program, the type of w is (3) , the type of x is (4) and the type of xs is (5) . If we run f and print the value of xs to the screen we would see (6) .
-}
-- (1) Nothing 
-- (2) Just 
-- (3) Int 
-- (4) Int 
-- (5) Num a => [(a, Char)]
-- (6) [(1,'a'), (1,'b'), (2,'a'), (2,'b')]

-- c. Convert f above to bind form 
f w = g w >>= \x -> 
        let xs = [1, 2] >>= \z -> ['a','b'] >>= \v -> return (z, v) in h (snd (head xs)) >>= \y -> 
            return y

-- d. Convert the definition of xs to a list comprehension 
[(z, v) | z <- [1, 2], v <- ['a', 'b']]

-- e. What does the following expression evaluate to?
do z <- [1, 2]
   v <- ['a', 'b']
   return v 

['a','b','a','b']



--------------------------------
-- IO 
--------------------------------

-- Let 'extract :: Int -> [a] -> ([a],a,[a])' be a function that given an index 'i' and a list 'xs' extracts the ith element from a list. More precisely, it can be implemented as
extract i xs = let pref = take i xs
                   (x:suf) = drop i xs in 
                       (pref,x,suf)

-- Given this function 'extract' we can implement the following function, which shuffles a list:
shuffle' :: [a] -> IO [a]
shuffle' [] = return []
shuffle' xs = do i <- randomRIO (0,length xs - 1)
                let (pref,x,suf) = (extract i xs)
                xs' <- shuffle' (pref ++ suf)
                return (x:xs')

-- a. Write a function 'foo :: IO Int' that asks the user to input one or more Ints separated by spaces, and prints a random permutation of this list and returns its sum.
-- Hint: the function 'getLine :: IO String' reads a line from the standard input
-- and the function 'putStrLn :: String -> IO ()' prints a string to standard output
-- and the function 'words :: String -> [String]' creates an array of string from the original one, white space characters serving as separators
-- and the function 'read :: String -> Int' can be used to convert a String into an Int


foo :: IO Int 
foo = do 
    putStrLn "Please input one or more Ints separated by spaces"
    w <- getLine 
    let ws = words w 
    ws' <- shuffle' ws 
    let w' = concat ws'
    putStrLn w'
    -- OR mapM_ putStrLn ws'
    -- OR sequence_ (map putStrLn ws')
    let is = map read ws 
    return (sum is)

-- b. Write a function of type [FilePath] −> FilePath −> IO () which concatenates a list of files and writes it to a specific target file: the first parameter is a list of filenames and the second parameter the name of the target file. Do not use the function appendFile. Don't worry about the situation in which the target file is one of the source files.

-- You may use 
-- readFile :: FilePath -> IO String 
-- writeFile :: FilePath -> String -> IO () 
-- to read and write files, respectively. 

concatFiles :: [FilePath] -> FilePath -> IO () 
concatFiles ifps ofp = do 
    ifs <- mapM readFile ifps 
    writeFile ofp (concat ifs)

-- OR (depending on how you interpret the question)

concatFiles' ifps ofp = do 
    ifcs <- mapM readFile ifps 
    ofc <- readFile ofp
    let temppath = ofp ++ "_temp" -- can do something fancier here to ensure we have a fresh path 
    writeFile temppath (ofc ++ concat ifcs)
    removeFile ofp 
    renameFile temppath ofp



--------------------------------
-- Laziness and execution
--------------------------------

-- a. Write down the reduction sequences for 
-- foldr (+) 0 [1,2,3]
-- foldl (+) 0 [1,2,3]
-- -foldl' (+) 0 [1,2,3]

-- foldr (+) 0 [1,2,3]
-- 1 + foldr (+) 0 [2,3]
-- 1 + (2 + foldr (+) 0 [3])
-- 1 + (2 + (3 + foldr (+) 0 []))
-- 1 + (2 + (3 + 0))
-- 1 + (2 + 3)
-- 1 + 5 
-- 6

-- foldl (+) 0 [1,2,3]
-- foldl (+) (0+1) [2,3]
-- foldl (+) ((0+1)+2) [3]
-- foldl (+) (((0+1)+2)+3) []
-- ((0+1)+2)+3
-- (1 + 2) +3
-- 3 + 3 
-- 6

-- foldl' (+) 0 [1,2,3]
-- foldl' (+) (0+1) [2,3]
-- foldl' (+) 1 [2,3]
-- foldl' (+) (1+2) [3]
-- foldl' (+) 3 [3]
-- foldl' (+) (3+3) []
-- foldl' (+) 6 []
-- 6

-- b. How does the resource use differ between foldr, foldl and foldl' (particularly, if we replace [1,2,3] with a large list)?
-- Time: linear all of them 
-- Space: foldr and foldl linear, foldl' constant


--------------------------------
-- Data types 
--------------------------------


-- a. Define a parameterized type Set a that consists of elements of type a, and define functions
subset :: Eq a => Set a -> Set a -> Bool -- that checks whether all the elements in the first set also belong to the second
empty :: Set a -- the empty set
insert :: Ord a => a -> Set a -> Set a -- inserting an element in a set
member :: Ord a => a -> Set a -> Bool -- checking membership of an element in a set


newtype Set a = MkSet [a] 
empty = MkSet [] 
member a (MkSet as) = elem a as 
insert a s@(MkSet as) | member a s = MkSet (a:as)
                      | otherwise = s 
subset (MkSet as) (MkSet as') = all (\a -> elem a as') as
                     


-- b. Use the subset function above to define an Eq instance for Set a.
instance Eq a => Eq (Set a) where 
    s == s' = subset s s' && subset s' s

-- c. Why do we have to define Set a as its own data type, instead of an alias over [a]?
-- Because [a] already has an Eq instance which is not the one we want (depending on how we define insert)



--------------------------------
-- Testing
--------------------------------
-- Consider the following datatype definition for binary trees that we shall want to use to implement binary search trees:

data Tree a = Branch a (Tree a) (Tree a) | Leaf deriving Show
-- a. Write a function

-- Method 1
isSearchTree :: Ord a => Tree a -> Bool -- probably the simplest and efficient
isSearchTree = isSorted . flattenInfix 

flattenInfix' :: [a] -> Tree a -> [a] -- basically enumInfix from about, but more efficient
flattenInfix' acc Leaf = acc
flattenInfix' acc (Branch a l r) = flattenInfix' (a : flattenInfix' acc r) l

flattenInfix :: Tree a -> [a] 
flattenInfix = flattenInfix' []

{- we could also just define 
flattenInfix Leaf = [] 
flattenInfix (Branch v l r) = flattenInfix l ++ [v] ++ flattenInfix r
if we don't care about efficiency
-}

isSorted :: Ord a => [a] -> Bool
isSorted (x:xs'@(x':xs)) = x <= x' && isSorted xs'
isSorted _ = True

-- Method 2: doesn't work!
isSearchTreeWrong :: Ord a => Tree a -> Bool  -- incorrect!
isSearchTreeWrong Leaf = True 
isSearchTreeWrong (Branch v l r) = isSearchTreeWrong l && isSearchTreeWrong r && ok (root l) (Just v)  && ok (Just v) (root r) where 
    ok Nothing _ = True 
    ok _ Nothing = True 
    ok (Just v) (Just v') = v <= v' 
    root Leaf = Nothing 
    root (Branch v _ _ ) = Just v


-- Method 3: fixing method 2 to work, but ending up with an inefficient algorithm
isSearchTreeInefficient :: Ord a => Tree a -> Bool 
isSearchTreeInefficient Leaf = True 
isSearchTreeInefficient (Branch v l r) = isSearchTreeInefficient l && isSearchTreeInefficient r && ok (treeMax l) (Just v)  && ok (Just v) (treeMin r) where 
    ok Nothing _ = True 
    ok _ Nothing = True 
    ok (Just v) (Just v') = v <= v' 
    treeMax Leaf = Nothing 
    treeMax (Branch v l r) = okMax (Just v) (okMax (treeMax l) (treeMax r))
    okMax Nothing v = v 
    okMax v Nothing = v 
    okMax (Just x) (Just y) = Just (max x y)
    treeMin Leaf = Nothing 
    treeMin (Branch v l r) = okMin (Just v) (okMinx (treeMin l) (treeMin r))
    okMin Nothing v = v 
    okMin v Nothing = v 
    okMin (Just x) (Just y) = Just (min x y)

-- Method 4: making method 3 more efficient by tupling in the min and max calculations
isSearchTreeEfficient :: Ord a => Tree a -> Bool 
isSearchTreeEfficient t = let (x, _, _) = isSearchTreeMinMax t in x

isSearchTreeMinMax :: Ord a => Tree a -> (Bool, Maybe a, Maybe a) 
isSearchTreeMinMax Leaf = (True, Nothing, Nothing)
isSearchTreeMinMax (Branch v l r) = (lb && rb && ok lmax (Just v) && ok (Just v) rmin, combine lmin (Just v), combine rmax (Just v)) where
        (lb, lmin, lmax) = isSearchTreeMinMax l 
        (rb, rmin, rmax) = isSearchTreeMinMax r 
        ok Nothing _ = True 
        ok _ Nothing = True 
        ok (Just v) (Just v') = v <= v'
        combine Nothing x = x 
        combine x _ = x


-- An example and counterexample to test on
example = Branch 5 (Branch 4 (Branch 3 Leaf Leaf ) (Branch 5 Leaf Leaf)) (Branch 6 (Branch 5 Leaf Leaf) (Branch 7 Leaf Leaf))
counterExample = Branch 5 (Branch 4 (Branch 3 Leaf Leaf ) (Branch 6 Leaf Leaf)) (Branch 6 (Branch 5 Leaf Leaf) (Branch 7 Leaf Leaf))


-- b. Write a function  
insertTree :: Ord a => a -> Tree a -> Tree a
insertTree a Leaf = Branch a Leaf Leaf
insertTree a (Branch a' l r) | a <= a' = Branch a' (insertTree a l) r
                             | otherwise = Branch a' l (insertTree a r)
-- that inserts an element in a tree, respecting the binary search tree property


-- c. Give an Arbitrary instance for Tree a to construct binary 
-- search trees
arbitraryL :: Arbitrary a => Gen [a]
arbitraryL = arbitrary 
instance (Ord a, Arbitrary a) => Arbitrary (Tree a) where 
    arbitrary = fmap (foldr insertTree Leaf) arbitraryL

-- d. Use quickCheck to test that your implementation of insertTree respects the search tree property
test :: Tree Int -> Bool
test = isSearchTree 

runTest :: IO ()
runTest = quickCheck test