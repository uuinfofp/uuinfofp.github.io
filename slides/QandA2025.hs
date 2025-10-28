module Main where

{-
  This file describes the topics covered in the Q&A Lecture. Most of
  the questions resolved around using/implementing IO/monads.

  I've left in the answers that we developed through the lecture. Note
  that this is/may not compile as a complete haskell module as is (as
  I've left in some duplicated/repeated function/class definitions),
  as well as additional type annotations to clarify the explanations.
-}

-- 1) *Using* IO
-- 2) Desugering do-notation
-- 3) Making a type a Monad
-- 4) Using Monads using Do notation; part 2
-- 5) Laziness

--------------------------------------------------------------------------------
-- * Using Do-notation (in IO)

{-

Question:
Given a FilePath to some textfile ; write some code that asks the user
for a word, and then prints the number of occurrences of that word in
the text file.
-}

countOccurences                 :: String -> String -> Int
countOccurences []     _        = error "I guess I should have picked a better type signature...."
countOccurences needle haystack = 5 -- TODO: implement this yourself ;)


countOccurencesInteractive    :: FilePath -> IO ()
countOccurencesInteractive fp = do putStrLn "input a  word:"
                                   word <- getLine
                                   contents <- readFile fp
                                   let n = countOccurences word contents
                                   putStrLn (show n)

{- Question:
   What if we want to keep asking the user for new words until they input the empty string
-}

countOccurencesInteractive'    :: FilePath -> IO ()
countOccurencesInteractive' fp = do contents <- readFile fp
                                    go contents
  where
    go contents = do putStrLn "input a  word:"
                     word <- getLine
                     if null word then return ()
                                  else do let n = countOccurences word contents
                                          putStrLn (show n)
                                          go contents

--------------------------------------------------------------------------------
-- * Monads; Desuggaring Do notation

-- Recall Monad is just a typeclass. It more or less looks like:

class Applicative m => Monad m where
    -- main two functions:
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

    -- it also has some additional functions, among which:
    (>>) :: m a -> m b -> m b
    ma >> mb = ma >>= \_ -> mb


--------------------------------------------------------------------------------

-- Question: Desuger the do-notation in countOccurencesInteractive'; i.e. use return, >>=, (and possibly >>)

countOccurencesInteractive'    :: FilePath -> IO ()
countOccurencesInteractive' fp = readFile fp >>= \contents -> go contents
                                 -- do contents <- readFile fp
                                 --    go contents
  where
    go contents = putStrLn "input a  word:" >>
                        getLine >>= \word ->
                          if null word then return ()
                                       else let n = countOccurences word contents
                                            in putStrLn (show n) >>
                                                 go contents
    -- go contents = do putStrLn "input a  word:"
    --                  word <- getLine
    --                  if null word then return ()
    --                               else do let n = countOccurences word contents
    --                                       putStrLn (show n)
    --                                       go contents


--------------------------------------------------------------------------------
-- * Implementing Monad

-- again recall the Monad Typeclass:
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

-- Now consider the following type 'Reader r a':
newtype Reader r a = MkReader (r -> a)

{-
Question: Make 'Reader r' an instance of Monad
-}


instance Monad (Reader r) where
  -- With these kinds of questions, consider the following strategy:
  --
  -- step 1: Figure out what types the functions have in this specific instance
  -- step 2: work from back to front; i.e. figure out how you can construct something of the return type; then figure out how you would build arguments/subexpressions you need along the way.

  -- for example:


  -- return :: a -> Reader r a    -- step 1
  return x = MkReader f           -- step 2a: we have to construct something of type 'Reader r a'
                                  -- so we haveto use a 'MkReader' constructor. We have to give the
                                  -- reader constructor something (i.e. an 'f') of type r -> a
    where
      f :: r -> a                 -- step 2b: let's try to construct an f of the right format
      f _ = x                     -- step 2c: we apparently need to construct something of type a;
                                  -- there is only one such value in scope: x.

  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (MkReader f) >>= k = MkReader func
    where
      func     :: r -> b
      func myR = let a = f myR
                     MkReader (foo :: r -> b) = k a
                 in foo myR

    -- same here; we need to construct a value of type 'Reader r b'. So then we have to construct
    -- some function 'func' of type r -> b.

    -- the only way to get your hands on something that involves a b
    -- is to use this 'k' function.  that gives me a thing of type
    -- 'Reader r b' though; that Reader thing is essentially a
    -- function 'foo' from type r -> b. So if I actually have this
    -- foo, I can use 'myR' (the only value of type r I have in scope)
    -- to produce a 'b'.
    --
    -- in order to use k, I actually need to have a thing of type a. Luckily, I can construct
    -- such an a by using the function f (and applying it to myR; the only r I have in scope).

----------------------------------------
-- * Some preliminaries

{-
  Recall that for 'Reader r' to be a Monad, it actually also has to be a (= an instance of) Functor, and Applicative. So, use the same scheme as above to implement those.
-}

-- recall that this is what the Functor class looks like:
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor (Reader r) where
-- so that means that we actually have to implement a function of type:

  fmap :: (a -> b) -> Reader r a -> Reader r b
    -- g :: r -> a
  fmap f (MkReader g) = MkReader $ \r -> f (g r)   -- following our backward reasoning scheme

  -- this is just the same as:
  -- fmap f (MkReader g) = MkReader $ f . g

-- we also have to be Applicative:

instance Applicative (Reader r) where
  pure = return
  -- <*> :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) = appReader
  -- TRY to implement <*> yourself. I've already given you the type it should have.
  -- the answer is at the bottom ofthis file.

  -- BONUS: You can cactually implement this generically using
  -- >>= and return (or possibly >>= and fmap)

----------------------------------------
-- * What is the point of Reader ?

{-
Question: Implement the following function 'ask' of type 'Reader r r'.
-}

ask :: Reader r r
ask = MkReader id

{-
A 'Reader r a' is just a function from r to a. You can think of a
'Reader r a' as a computation that produces an 'a' while using some additional context of type r.

this can be useful e.g. if you have some big application, and somewhere in some internals you need to access some (global) context; for example, a configuration of some sort. E.g., something like:

-}

data Config = Config { serverPort :: Int
                     , ...
                     }


-- | Given a hostname, return the URL to the hostname that listens at the correct port.
computeURL          :: String -> Reader Config String
computeURL hostName = do config <- ask
                         return $ "http://" ++ hostName ++ ":" ++ show (serverPort config)

-- I made remark of there being only one possible implementation of type 'r -> r'.
-- (as the r is universally quantified; i.e. we don't know anything about the r.)
bar :: forall r.  r -> r
bar myR = myR

----------------------------------------
-- * Using Do Notation (for Maybe)

{-
   Monads/do-notation is useful for all sorts of types. Here was some
   example exam question that asks you to use do-notation to implement some combineLookup function:

-}

-- Recall that

-- 1. a

data Map k v = ...

  -- is a data structure that associates keys of type k with values of type v,

-- and allows us to efficiently retrieve the
-- value associated with a key, if it exists, using the function

lookup :: Ord k => k -> Map k v -> Maybe v

--  If the key does not occur in the Map, lookup returns a Nothing.


-- 2. Maybe is an instance of Monad.

-- Consider a function combineLookup :: Ord k => (v -> v -> Maybe b)
-- -> k -> k -> Map k v -> Maybe b that looks up two keys in a Map k v
-- (using the lookup function), and combines their values using a user
-- supplied function.


-- Here are some example uses of combineLookup, in which m = Map.fromList [(1,"foo"), (2,
-- "bar"), (6,""), (8,"baz")] is a Map that maps the key 1 to ”foo”, 2 to ”bar” etc.
-- Page 4(b) (c) > combineLookup (\v1 v2 -> Just (v1 ++ v2)) 1 2 m
-- Just "foobar"
-- > combineLookup (\v1 v2 -> if null v1 then Just v2 else Nothing) 1 2 m
-- Nothing
-- > combineLookup (\v1 v2 -> if null v1 then Just v2 else Nothing) 6 2 m
-- Just "bar"
-- > combineLookup (\v1 v2 -> if null v1 then Just v2 else Nothing) 3 2 m
-- Nothing
-- Using do-notation, please implement combineLookup.


combineLookup           :: Ord k => (v -> v -> Maybe b) -> k -> k -> Map k v -> Maybe b
combineLookup f k1 k2 m = do v1 <- lookup k1 m
                             v2 <- lookup k2 m
                             f v1 v2

-- The implementation above is equivalent, but much shorter than:
...                     = case lookup k1 m of
                            Nothing -> Nothing
                            Just v1 -> case lookup k2 m of
                                          Nothing -> Nothing
                                          Just v2 -> f v1 v2


--------------------------------------------------------------------------------
-- * There was a question about Monoids

-- Monoid is just a typeclass describing that some type t has an unit
-- element "mempty", and a binary operation <> that allows you to
-- combine two elements. There are laws that require e.g. that
-- mempty <> x = x, and that x <> mempty = x
class Monoid t where
  mempty :: t
  (<>)   :: t -> t -> t

-- the prototypical example is: lists; they are a monoid:
instance Monoid [a] where
  mempty = []
  (<>) ls rs = ls ++ rs

-- Int's can be made an instance of Monoid as well; e.g. using 'mempty
-- = 0', and (<>) = (+).  however, there is an alternative of choosing
-- mempty = 1, and (<>) = (*) as well.  (So by default haskell's
-- standard library does not choose either).

--------------------------------------------------------------------------------
-- * Laziness

--  For each of these expressions, indicate if they are in WHNF or not. For the ones that
-- are in WHNF, state in one sentence why. For the ones not in WHNF, evaluate them to WHNF or,
-- in case they crash upon evaluation, indicate this.

-- A. (1 + 5) : succ 4 : map (+1) [1,2]

-- is in WHNF; since : is a constructor


-- B. isNothing (Just 4)

-- not WHNF; evaluates to False

-- C. \a b c -> and b

-- WHNF; because lambda function (with missing args)

-- D. foldr undefined e []
-- E. seq fmap

-- TODO :)

-- F. seq (undefined, map (+1) [1..]) 0

-- not in WHNF; first arg to seq is already in whnf, so evaluating it does not do anything.


-- G': Evaluate the following expression to WHNF (using a minimum number of steps:)

map (+1) [1..]

-- after essentially one step:

((+1) 1) : map (+1) [2...]






































--------------------------------------------------------------------------------
-- * The answer to our <*> for 'Reader r' question:

-- f :: r -> (a -> b)
-- x :: r -> a
(MkReader f) `appReader` (MkReader x) = MkReader $ \r -> f r (x r)



main = print "woei"
