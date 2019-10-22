module Lecture10 where

import           Control.Monad
    -- from last time: IO
import           Data.Map
import           Data.Time
import           System.Environment
import           System.Random

type Time = TimeOfDay

type Disk = Map String String

type Console = String

type Keyboard = Char

type Args = [String]

data World =
  MkWorld Time Disk Console Keyboard Args
    -- Note that World really is inaccessible in Haskell! But you can think of it something like this.

type IO' a = World -> (a, World)

main' :: IO' ()
main' = undefined

unsafePerformIO' :: IO' a -> a -- ONLY AT VERY OUTSIDE OF PROGRAM, after main, wires up operations on World to syscalls to actually read and write to the state of the computer.
unsafePerformIO' = undefined

-- this time: Functors and Monads

data Tree a = Leaf
            | Node (Tree a) a (Tree a) deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf         = Leaf
mapTree f (Node l v r) = Node (mapTree f l) (f v) (mapTree f r)


mapMay :: (a -> b) -> Maybe a -> Maybe b
mapMay f Nothing  = Nothing
mapMay f (Just a) = Just (f a)

-- Functors as higher-kinded abstraction
-- programs :: types :: kinds
-- higher kinds

-- Int :: *
-- Bool :: *
-- [] :: * -> *
-- Maybe :: * -> *
-- Map :: * -> * -> *

data HigherOrderTypeFormer f a = Val a (f a)
-- HigherOrderTypeFormer :: (* -> *) -> * -> *
-- any functor :: * -> *


instance Functor ((->) r) where
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = f . g


instance Functor IO where
-- fmap :: (a -> b) -> IO a -> IO b
    fmap f ia = do
        a <- ia
        return (f a)

-- map/Functor intuition: keep shape of container the same but transform the values it holds by applying function (in the sense that functor laws hold).


-- Recall that IO represents computation with io-side effects.

-- What notion of computation does Maybe represent?


-- How is each monad a functor?
fmap :: Monad m => (a -> b) -> m a -> m b
fmap f ma = ma >>= \a -> return (f a)



f :: Maybe Int -> Maybe Int
f m = do
    x <- m
    return 3
    return (x + 1)
-- is equivalent to --   
f' m = do
    x <- m
    _ <- return 3
    return (x + 1)
-- is equivalent to --  
f'' m = do
    x <- m
    return (x + 1)

fNothing m = do
    _ <- Nothing
    x <- m
    return (x + 1)
-- is equivalent to --
fNothing' m = Nothing >>= \_ -> m >>= \x -> return (x + 1)
-- is equivalent to --
fNothing'' m = Nothing `maybeBind` \_ -> m `maybeBind` \x -> return (x + 1)  where
    maybeBind Nothing  _ = Nothing  -- recall definition of >>= for Maybe, as an aside for those who like math: Nothing should be sent to Nothing because we generally want >>= to be a homomorphism. (In this case, note that there also isn't much of another option.)
    maybeBind (Just x) f = f x
-- is equivalent to -- 
fNothing2 m = maybeBind Nothing (\_ -> m `maybeBind` \x -> return (x + 1))  where
    maybeBind Nothing  _ = Nothing
    maybeBind (Just x) f = f x
-- is equivalent to -- 
fNothing3 m = Nothing -- by substituting definition of maybeBind

g :: Maybe Int -> Maybe Int
g m = do
    x <- return 3
    y <- m
    return (x + y)

g' :: Maybe Int -> Maybe Int
g' m = do
    x <- Just 3
    y <- m
    return (x + y)

instance Monad [] where
    -- return :: a -> [a]
    return x = [x]
-- (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = concat (map f xs)


mzero' :: Maybe a
mzero' = Nothing
mplus' j@(Just a) _ = j
mplus' _          b = b

-- What notion of computation does List represent?

filter' :: MonadPlus m => (a -> Bool) -> m a -> m a
filter' p ma = ma >>= \a -> perhaps a  where
    perhaps a | p a       = return a
              | otherwise = mzero

filter'' p ma = do
    a <- ma
    perhaps a  where
    perhaps a | p a       = return a
              | otherwise = mzero
