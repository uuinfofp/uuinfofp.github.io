module Lecture9 where 

import           System.Environment
import           System.Random
import           Data.Time
import           Data.Map

import           Prelude                 hiding ( (>>=)
                                                , return
                                                )

(>>>) = flip (.)

-- What does it mean for two programs to be equivalent?
-- At type Bool, Int, Float?
-- At function type?
-- How should we think of World?
-- How should we read IO a? b -> IO a?
type Time = TimeOfDay

type Disk = Map String String

type Console = String

type Keyboard = Char

type Args = [String]

data World =
  MkWorld Time Disk Console Keyboard Args -- Note that world really is inaccessible in Haskell! But you can think of it something like this.

type IO'' = World -> World

putChar'' :: Char -> IO'' -- Pretending that IO and World look like that, let us implement putChar.
putChar'' c' (MkWorld t d c k a) = MkWorld t d (c' : c) k a

type IO' a = World -> (a, World) -- More refined look at IO

putChar' :: Char -> IO' ()
putChar' c' w@(MkWorld _ _ c _ _) = ((), putChar'' c' w)

getChar' :: IO' Char -- Pretending that IO and World look like that, let us implement getChar.
getChar' w@(MkWorld _ _ _ k _) = (k, w)


(>>=) :: IO' a -> (a -> IO' b) -> IO' b -- Pretending that IO and World look like that, let us implement >>=.
(f >>= g) w = let (a', w') = f w in g a' w'

echo :: IO' ()
echo = getChar' >>= \c -> putChar' c


return :: a -> IO' a -- Pretending that IO and World look like that, let us implement return.
return a w = (a, w)

putStr' :: String -> IO' () -- without do
putStr' []     = return ()
putStr' (c:cs) = putChar' c >>= (\_ -> putStr' cs)

putStr'' :: String -> IO' () -- with do
putStr'' [] = return ()
putStr'' (c:cs) = do
  putChar' c
  putStr'' cs

getArgs' :: IO' [String]
getArgs' w@(MkWorld _ _ _ _ a) = (a, w)

unsafePerformIO' :: IO' a -> a -- evil, for an idea of what unsafePerformIO does: it converts any reads and writes on World to the corresponding syscalls to actually read and write the state of the machine
unsafePerformIO' f = undefined

runMain :: IO' () -> () -- evil
runMain = unsafePerformIO'

sequence_' :: [IO' a] -> IO' () -- with do
sequence_' [] = return ()
sequence_' (ia:ias) = do
  ia
  sequence_' ias

sequence' :: [IO' a] -> IO' [a] -- without do
sequence' []       = return []
sequence' (ia:ias) = ia >>= \a -> sequence' ias >>= \as -> return (a : as)

liftM2' :: (a -> b -> c) -> IO' a -> IO' b -> IO' c -- without do
liftM2' f ioa iob = ioa >>= \a -> iob >>= \b -> return (f a b)

generateTwoNumbers :: RandomGen g => g -> ((Int, Int), g)
generateTwoNumbers g =
  let (i1, g1) = random g
      (i2, g2) = random g1
   in ((i1, i2), g2)
