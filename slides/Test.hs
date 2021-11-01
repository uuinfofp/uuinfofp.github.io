

--------------------------------
-- Equational reasoning, induction
--------------------------------
-- We first ask you to prove some equations.
-- In your proofs, please motivate every reasoning step with 
-- either a definition, a law, or the use of an induction hypothesis.

-- a.
-- Some useful definitions:
data Tree a = Leaf a | Node (Tree a) a (Tree a)

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



-- b. Show that the Maybe monad satisfies the monad laws, where you 
-- may use the definitions 
return = Just 
Nothing >>= _ = Nothing 
(Just x) >>= f = f x



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






--------------------------------
-- Monads
--------------------------------
-- Consider the data type
data Reader s a = MkReader (s -> a)

-- a. Make Reader s an instance of Monad





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


-- c. Convert f above to bind form 

-- d. Convert the definition of xs to a list comprehension 

-- e. What does the following expression evaluate to?
do z <- [1, 2]
   v <- ['a', 'b']
   return v 




--------------------------------
-- IO 
--------------------------------

-- Let 'extract :: Int -> [a] -> ([a],a,[a])' be a function that given an index 'i' and a list 'xs' extracts the ith element from a list. More precisely, it can be implemented as
extract i xs = let pref = take i xs
                   (x:suf) = drop i xs in 
                       (pref,x,suf)

-- Given this function 'extract' we can implement the following function, which shuffles a list:
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do i <- randomRIO (0,length xs - 1)
                let (pref,x,suf) = (extract i xs)
                xs' <- shuffle (pref ++ suf)
                return (x:xs')
-- a. Write a function 'foo :: IO Int' that asks the user to input one or more Ints separated by spaces, and prints a random permutation of this list and returns its sum.
-- Hint: the function 'getLine :: IO String' reads a line from the standard input
-- and the function 'putStrLn :: String -> IO ()' prints a string to standard output
-- and the function 'words :: String -> [String]' creates an array of string from the original one, white space characters serving as separators
-- and the function 'read :: String -> Int' can be used to convert a String into an Int


-- b. Write a function of type [FilePath] −> FilePath −> IO () which concatenates a list of files to a specific target file: the first parameter is a list of filenames and the second parameter the name of the target file. Do not use the function appendFile. Don't worry about the situation in which the target file is one of the source files.

-- You may use 
-- readFile :: FilePath -> IO String 
-- writeFile :: FilePath -> String -> IO () 
-- to read and write files, respectively. 




--------------------------------
-- Laziness and execution
--------------------------------

-- a. Write down the reduction sequences for 
foldr (+) 0 [1,2,3]
foldl (+) 0 [1,2,3]
foldl' (+) 0 [1,2,3]

-- b. How does the resource use differ between foldr, foldl and foldl' (particularly, if we replace [1,2,3] with a large list)?



--------------------------------
-- Data types 
--------------------------------


-- a. Define a parameterized type Set a that consists of elements of type a, and define functions
subset :: Eq a => Set a -> Set a -> Bool -- that checks whether all the elements in the first set also belong to the second
empty :: Set a -- the empty set
insert :: Ord a => a -> Set a -> Set a -- inserting an element in a set
member :: Ord a => a -> Set a -> Bool -- checking membership of an element in a set


-- b. Use the subset function above to define an Eq instance for Set a.


-- c. Why do we have to define Set a as its own data type, instead of an alias over [a]?




--------------------------------
-- Testing
--------------------------------
-- Consider the following datatype definition for binary trees that we shall want to use to implement binary search trees:

data Tree a = Branch a (Tree a) (Tree a) | Leaf
-- a. Write a function
isSearchTree :: Ord a => Tree a -> Bool
-- that verifies that its argument is a binary search tree. 



-- b. Write a function  
insertTree :: Ord a => a -> Tree a -> Tree a
-- that inserts an element in a tree, respecting the binary search tree property


-- c. Give an Arbitrary instance for Tree a to construct binary 
-- search trees


-- d. Use quickCheck to test that your implementation of insertTree respects the search tree property


