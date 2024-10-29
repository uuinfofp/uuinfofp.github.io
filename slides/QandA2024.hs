{- cabal:
build-depends: base, mtl
-}
module Main where

import Control.Monad.State

{-
  cabal repl QandA2024.hs
-}

--------------------------------------------------------------------------------
-- * The curry proof question

{-

1)
prove that:

curry (f . swap) = flip (curry f)

swap :: (a,b) -> (b,a)
f :: (b,a) -> c

f . swap :: (a,b) -> c

curry :: ( (a,b) -> c    ) ->     (     a ->   b   -> c   )

curry (f . swap) :: a -> b -> c

flip (curry f) :: a -> b -> c


for all (x :: a), (y :: b)

thus, by extentionality, we have to prove that : for all x, and y:

curry (f . swap) x y    ==  flip (curry f)   x  y


  curry (f . swap) x y
= { g}
  (f . swap) (x,y)
= { h }
  f (swap (x,y))
= { e }
  f (y,x)
= {g }
  curry f y x
= { haakjes }
  (curry f) y x
= { f }
  flip (curry f) x y


dus curry (f . swap) = flip (curry f)














-}

{-
- do notatie desuggaren
- modify state monad (voorbeeld)
- random?
- iets met monads
- WHNF
- functor, applicative, monad
-
-}



--------------------------------------------------------------------------------

{-
Given functions

listDirectory :: FilePath -> IO [FilePath]
readFile :: FilePath -> IO String

write an program 'largeFiles' that given a FilePath to a directory, prints all filenames
of files in that directory that are large. A file is large when it has at least 200 lines.

-}


isLargeFile :: FilePath -> IO Bool
isLargeFile path = do content <- readFile path
                      let n = length $ lines content
                      return $ n >= 200

--------------------------------------------------------------------------------
------------ Exercise
--
-- Write isLargeFile without using do notation and >>=. Instead, use that IO
-- is an instance of Functor instead.
--------------------------------------------------------------------------------


largeFiles :: FilePath -> IO [String]
largeFiles dir = do files      <- listDirectory dir
                    largeFiles <- filterM isLargeFile files
                    return largeFiles

filterM          :: Monad m => (c -> m Bool) -> [c] -> m [c]
filterM p []     = return []
filterM p (x:xs) = do b    <- p x
                      rest <- filterM p xs
                      let res = if b then (x:rest) else rest
                      return res


{-
NOTE: The 'listDirectory' function from the Directory package also returns fileNames such
as '.' and '..'. We probably want ot filter those out as well.
-}

listDirectory = undefined


--------------------------------------------------------------------------------
------------ Exericse
--
-- write filterM as a foldr.
--
-- Hint: Recall that foldr has type :: (a -> b -> b) -> b -> [a] -> b. What is the
-- concrete type that we fill in for 'b' here?
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- * Functor and Monad


-- As promissed here is the proof/argument that you can implement Functor using monad:

fmapViaMonad      :: Monad m => (a -> b) -> m a -> m b
fmapViaMonad f ma = ma >>= \a -> return (f a)


--------------------------------------------------------------------------------
-- * State  Monad

{- We import the definition of the State monad from the MTL package (see above) This implementation gives us the following functions:

  Note that the runState type from the mtl package returns a pair of type (a,s)  instead of a pair of type (s,a) (as in the slides).

  - get      :: State s s
  - put      :: s -> State s ()
  - modify   :: (s -> s) -> State s ()
  - runState :: State s a -> s -> (a, s)
 -}


{- Consider the following type of colorful trees: -}
data Color = Red | Blue | Black deriving (Show,Eq)

data Tree a b = Leaf Color a | Node (Tree a b) b (Tree a b)
  deriving (Show,Eq)

myTree = Node (Node (Leaf Blue "foo") 1 (Leaf Red "bar")) 5 (Leaf Red "go")

{-
  Our goal is to write a function 'nthRed :: Int -> Tree a b -> Maybe a' that given an
  index n, and a tree, computes the nth red leaf (as considered in an in-order traversal)
  if it exists.

  We will show how to we can do so using a State monad. Note that there is a simple
  solution just using "regular" functions; so this is just for illustrative purposes.

  We will write a function 'countRed :: Int -> Tree a b -> State Int (Maybe a)' to this
  end. The idea is that the state parameter (the Int in 'State Int _') will maintain the
  number of red leaves seen so far. We can then implement nthRed using runState:

-}

nthRed :: Int -> Tree a b -> Maybe a
nthRed n t = fst $ runState (countRed n t) 0
  -- initially, we've seen 0 red nodes; 'runState' returns the final
  -- result (our Maybe a), as well as the final value of the state
  -- (in our case; the value 'n'). We simply ignore that value.



-- Here is the function/procedure that does actually all the work.
countRed                :: Int -> Tree a b -> State Int (Maybe a)
countRed n (Leaf Red x) = do i <- get -- number of reds seen so far
                             put (i+1)  -- we've now seen the i+1'st one.
                                        -- so update the state
                             return $ if (i+1) == n then Just x else Nothing
countRed _ (Leaf _ x)   = return Nothing -- no red leaf here; so just skip
countRed n (Node l _ r) = do resL <- countRed n l
                             resR <- countRed n r
                             return $ resL <|> resR
         -- we try to find the nth red leaf in the left subtree and
         -- then we try to find it in the right subtree.
         -- note that if it exists in the left subtree, we just ignore the
         -- resR result (and thus; by laziness it will actually never be computed)

  where
    Nothing    <|> r = r
    l@(Just _) <|> _ = l



-- instead of get and put we could have used modify, i.e.
--
countRed' n (Leaf Red x) = do modify $ \i -> i+1
                              newI <- get
                              return $ if newI == n then Just x else Nothing


--------------------------------------------------------------------------------
---------- Exercise
-- make 'Tree a'  an instance of Functor
--------------------------------------------------------------------------------

main = print "woei"
