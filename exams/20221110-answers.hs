{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Exam221110 where

-- import Control.Monad.Identity
import System.Directory
import Data.Char
import Control.Monad.State.Lazy
import qualified Data.List as List
--import Test.QuickCheck



--------------------------------------------------------------------------------
-- * 1. QuickCheck

type Guest = String
data Room = Room { roomNumber :: Int
                 , members    :: [Guest]
                 }
              deriving (Show,Eq,Ord)

-- | A floor consists of rooms (which all have an room number)
newtype Floor = Floor { rooms :: [Room] }
  deriving (Show)



-- a) Write a function that tests if all values in a list are
-- consecutive, i.e. every next element is the successor of the predecessor

-- consecutive []       = True
-- consecutive [x]      = True
-- consecutive (x:y:ys) = y == succ x && consecutive (y:ys)

consecutive    :: (Enum a, Eq a) => [a] -> Bool
consecutive xs = and $ zipWith (\x y -> y == succ x) xs (tail xs)

-- b) write a property that tests if there are any missing rooms;
-- i.e. if all room numbers of the rooms on a floor are consecutive.
--
-- you can use the function 'sort :: Ord a => [a] -> [a]'

noMissing :: Floor -> Bool
noMissing = consecutive . List.sort . map roomNumber . rooms


-- c) a floor is Valid, if no two rooms have the same number;
-- please define a function isValidFloor that checks whether a floor is valid


isValidFloor   :: Floor -> Bool
isValidFloor = not . any (\g -> length g > 1) . List.group . List.sort . map roomNumber . rooms


-- d) As code completion question:
-- given an infinite list rms of Room and an Int n , generate a valid floor that has n rooms taken from the list rms 
genValidFloor :: [Room] -> Int -> Floor 
genValidFloor rms n = Floor $ take n (validRms [] rms)
validRms acc (rm:rms) | elem (roomNumber rm) acc = validRms acc rms 
                      | otherwise = rm : validRms ((roomNumber rm):acc) rms




--------------------------------------------------------------------------------
-- * 2. Equational reasoning and induction


data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

exampleTree = Node (Node (Node (Leaf)(3)(Leaf)) (2) (Node (Leaf)(4)(Leaf))) (1) (Node (Node (Leaf)(6)(Leaf)) (5) (Node (Node (Node (Leaf)(9)(Leaf))(8)(Node (Leaf)(10)(Leaf)))(7)(Node (Leaf)(11)(Leaf))))




size :: Tree a -> Int
size Leaf = 0 -- a
size (Node l v r) = size l + (1 + size r) -- b


combineTree :: Tree a -> Tree a -> Tree a
combineTree Leaf r' = r' -- c
combineTree (Node l v r) r' = Node l v (combineTree r r') -- d

-- 0 + i = i -- law e
-- (i + j) + k = i + (j + k) -- law f


{-
Claim: for all Trees l' and r',   size (combineTree l' r') = size l' + size r'



Proof:
We distinguish the cases where l' = Leaf and l' = Node l v r.


Case l' = Leaf
size (combineTree Leaf r')
= c
size r'
= e
0 + size r'
= a
size Leaf + size r'


Case l' = Node l v r
We assume the induction hypotheses
size (combineTree l r') = size l + size r'    (I.H.1, which we won't need - in fact, we could leave this out, but I suspect a lot of students will write it down)
size (combineTree r r') = size r + size r'    (I.H.2, which we will need)


size (combineTree (Node l v r) r')
= d
size (Node l v (combineTree r r'))
= b
size l + (1 + size (combineTree r r'))
= I.H.2
size l + (1 + (size r + size r'))
= f
size l + ((1 + size r) + size r')
= f
(size l + (1 + size r)) + size r'
= b
size (Node l v r) + size r'

The claim now follows by indcution on l'.


-}


--------------------------------------------------------------------------------
-- * 3. Laziness

{-
Indicate, for each of the following expressions what their WHNF is. If the
expression is already in WHNF, please copy the original expression. If the
expression crashes in its evaluation to WHNF, please write "undefined".


a) foldr (\xs (a,as) -> (length xs + a, xs ++ as) ) (0,[]) [[1]]
(length [1] + 0, [1] ++ [])


b) map (+1) [0..2]
(+1) 0 : map (+1) [1, 2]


c) length xs : map (+1) [0..5]
length xs : map (+1) [0..5]

d) (\x -> Node Leaf (x + 1) Leaf) undefined
Node Leaf (undefined +1) Leaf
-}





--------------------------------------------------------------------------------
-- * 4. IO/Monads

-- a) Write a function 'countLinesAndWords' that takes a FilePath to
-- some text file, prints the number of lines in the file, and returns
-- the total number of words in the file.
--
-- Remember that 'readFile :: FilePath -> IO String' reads the given
-- file, and 'print :: Show a => a -> IO ()' print a value to standard
-- output.
--
countLinesAndWords    :: FilePath -> IO Int
countLinesAndWords fp = do s <- readFile fp
                           let ls = lines s
                           print (length ls)
                           return $ length (words s)



-- The following funtion longestFile reads a directory name, and uses
-- countLinesAndWords to compute the file with the most words. (it may
-- print some stuff on standard out in the mean time.)

longestFile    :: FilePath -> IO Int
longestFile fp = do files <- listDirectory fp
                    ls <- mapM countLinesAndWords files
                    print "Working..."
                    let l = maximum ls
                    return l

-- b) rewrite 'longestFile' to use >>= and return directly rather than
-- using do-notation.

longestFile' :: FilePath -> IO Int
longestFile' fp = listDirectory fp >>= \files ->
                    mapM countLinesAndWords files >>= \ls ->
                      print "Working..." >>= \_ ->
                        let l = maximum ls in return l



--------------------------------------------------------------------------------
-- 5. Lenses

{-
Consider the following data types:
-}

data Office = Office { _building :: String
                    , _floor    :: Int
                    , _room     :: Int
                    }
             deriving (Show,Eq)

data Employee = Employee { _name   :: String
                         , _age    :: Int
                         , _office :: Office
                         }
           deriving (Show,Eq)

wrongFrank = Employee "Frank" 34 (Office "BBG" 4 9)
matthijs = Employee "Matthijs" 32 (Office "BBG" 5 65)

frank = moveToRoom 11 wrongFrank

-- a) write a function moveToRoom which takes a room number, and an
-- employee, and updates the 'room' field of the office of that
-- employee. Your function should use pattern matching to access the
-- appropriate fields.

moveToRoom                                :: Int -> Employee -> Employee
moveToRoom r' (Employee n a (Office b f r)) = Employee n a (Office b f r')
--OR something like
moveToRoom' r' e@Employee{_office=o} = e {_office = o{_room=r'} }




{- Let us also define a type level equivalent of the 'Const' function,
 which takes two type arguments, and only remembers the first one.

-}

newtype Const c a = MkConst c
                  deriving (Show,Eq)

-- i.e. note that 'reallyJustAnInt' only stores an Int; the 'String'
-- in the type signature is not stored at all/there is no string at all.

-- >>> reallyJustAnInt
-- MkConst 5
reallyJustAnInt :: Const Int String
reallyJustAnInt = MkConst 5

-- b) Let c be some type. Give the Functor instance for the type "Const c"
--
-- i.e. we should have that:
--
stillNoString = fmap (++ "and some extra string") reallyJustAnInt

-- >>> stillNoString
-- MkConst 5


instance Functor (Const c) where fmap f (MkConst c) = MkConst c


{-
  While the solution to question a. is hopefully not too difficult,
  it is not very convenient to
  write the above code, (in particular if Employee and Office would both
  have many more fields).

  In this exercise, we investigate an alternative, more composable,
  solution to the problem called Lenses.

  Consider the following type 'LensF' which we can use as accessors of
  a particular field in a haskell data type. See also the example
  accessors below.

  Note that technically, a 'LensF' is just a function with two
  parameters.
-}

type LensF f p a = (a -> f a) -> p -> f p


name                  :: Functor f => LensF f Employee String
name f (Employee n a o) = fmap (\n' -> Employee n' a o) $ f n

age                  :: Functor f => LensF f Employee Int
age f (Employee n a o) = fmap (\a' -> Employee n a' o) $ f a

office                  :: Functor f => LensF f Employee Office
office f (Employee n a o) = fmap (\o' -> Employee n a o') $ f o

room                  :: Functor f => LensF f Office Int
room g (Office b f r) = fmap (\r' -> Office b f r') $ g r

-- c) Write the function 'view', so that we can use a 'myLens :: LensF
-- (Const a)' as a "getter" of a record. That is, so that 'view myLens p'
-- to view/read/get the field 'myLens' of a record 'p':
--
-- >>> view name frank
-- "Frank"
-- >>> view office matthijs
-- Office {_building = "BBG", _floor = 5, _room = 70}
--
--
view        :: LensF (Const a) p a -> p -> a
view lens p = let MkConst a = lens MkConst p
              in a

{-
  Using the following 'Identity' type, we can also write a "setter" function:


-}

superFrank = set name frank "FRANK"

newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- >>> set name frank "FRANK"
-- Employee {_name = "FRANK", _age = 34, _office = Office {_building = "BBG", _floor = 4, _room = 11}}
set           :: LensF Identity s a -> s -> a -> s
set lens s a' = let Identity s' = lens (\_ -> Identity a') s
                in s'

{-
 The nice thing about these Lenses is that they compose! To access the
 room number of a particular employee, we can now write:

 (Note: Don't worry why this actually works the way it does/why we
 can write the lens this way.)
-}

officeNumber :: Functor f => LensF f Employee Int
officeNumber = office . room

{-
d) Write a function 'moveToRoom2' that uses the 'officeNumber' lens to update the room of a particular employee:

-- >>> moveToRoom2 110 frank
-- Employee {_name = "Frank", _age = 34, _office = Office {_building = "BBG", _floor = 4, _room = 110}}
-}

moveToRoom2     :: Int -> Employee -> Employee
moveToRoom2 n p = set officeNumber p n


{-  Bonus
e) Make Identity an instance of Monad:
-}

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)
{- don't worry about this -}

instance Monad Identity where
  return = Identity
  (Identity x) >>= k = k x
