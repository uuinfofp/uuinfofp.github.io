---
title: Solutions to selected exercises
---

### Lectures 1 and 2 - Functions and types

#### Exercise 1

The number of solutions is given by the result of the discriminant, `b² - 4ac`.

```haskell
noOfSol a b c | discr >  0 = 2
              | discr == 0 = 1
              | otherwise  = 0
  where discr = b * b - 4 * a * c
```

#### Exercises 2 and 3

Remember that you can use `ghci` to ask for the type of an expression or its value:

```
> :t (++) [True, False]
append [True, False] :: [Bool] -> [Bool]
> [1,2] ++ [3,4]
[1,2,3,4]
```

#### Type inference

The types of `map . foldr` and `map (map map)` have been discussed in the Q&A session before the mid-term exam.


### Lecture 3 - Lists and recursion

#### Exercise 1

The evaluation of `factorial n` for any negative number loops indefinitely. The reason is that there is no base case to stop the recursion.

#### Exercise 2

```haskell
x ^ 0 = 1
x ^ n | n `rem` 2 == 0 = y * y
      | otherwise      = x * y * y
  where y = x ^ (n `div` 2)
```

#### Exercise 3

```haskell
last :: [a] -> a
last []     = error "empty list"
last [x]    = x
last (_:xs) = last xs

-- Another version which does not raise errors
last' :: [a] -> Maybe a
last' []     = Nothing
last' [x]    = Just x
last' (_:xs) = last xs
```

#### Exercise 5

In this solution I assume that the indices start at 0. E.g., `[1,2] !! 0 = 1`.

```haskell
(!!) :: [a] -> Int -> a
_      !! n
    | n < 0 = error "negative index"
[]     !! _ = error "not enough elements"
(x:_)  !! 0 = x
(x:xs) !! n = xs !! (n-1)
```

#### Exercise 6

```haskell
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
```

#### Exercises 8, 9 and 10

```haskell
remSuccessiveDuplicates :: Eq a => [a] -> [a]
remSuccessiveDuplicates []     = []
remSuccessiveDuplicates (x:xs) = x : remSuccessiveDuplicates' x xs
  where remSuccessiveDuplicates' :: Eq a => a -> [a] -> [a]
        remSuccessiveDuplicates' x [] = []
        remSuccessiveDuplicates' x (y:ys)
          | x == y    =     remSuccessiveDuplicates' x ys  -- We found the same element
          | otherwise = y : remSuccessiveDuplicates' y ys  -- Start with a new value                                      
```

Exercies 9 and 10 are a variation of exercise 8, in which you need to keep extra information. Note also that:

```haskell
runLengthEncoding = map (\x -> (length x, head x)) . group
```

#### Exercise 15

```haskell
split :: Int -> [a] -> [[a]]
split n xs = split' n xs
  where split' :: Int -> [a] -> [a]
        split' _ []     = []
        split' 0 xs     = split' n xs  -- Start again
        split' n (x:xs) = let (t:ts) = split' (n-1) xs in ((x:t):ts)
                                       -- Add one to the first list

-- Using built-in functions
split _ [] = []
split n xs = take n xs : split n (drop n xs)
```

### Lecture 4 - Higher-order functions

#### Exercise 1

```haskell
applyToZero :: (Float -> Float) -> Float
applyToZero f = f 0

add :: Float -> (Float -> Float)
add x = \y -> x + y
-- or taking currying into account
add x y = x + y
-- or even reducing the arguments
add = (+)

id :: (Float -> Float) -> (Float -> Float)
id f = f  -- identity works here

apply :: (Float -> Float) -> Float -> Float
apply f n = f n  -- apply 'f' to 'n'
```

#### Exercise 2

```haskell
concat = foldr (++) []
```

#### Exercise 4

The idea here is that we will turn a list into a lists of lists. Each of these inner lists will be either empty, if we want to remove the item, or just hold a single value if we want to preserve it. If we then flatten those inner lists with `concat`, we are done.

```haskell
filter p = concat . map box
  where box x | p x       = [x]
              | otherwise = []
```

#### Exercise 5

```haskell
before :: (a -> b) -> (b -> c) -> a -> c
before f g = \x -> g (f x)

-- Also, before is usual composition with its arguments flipped
-- (.) :: (b -> c) -> (a -> b) -> a -> c
before :: (a -> b) -> (b -> c) -> a -> c
before = flip (.)
```

#### Exercise 6

```haskell
-- The type of map for lists
map :: (a -> b) -> [a] -> [b]
-- We could also write it by putting [] in front
map :: (a -> b) -> [] a -> [] b
-- Now we substitute [] for (c ->)
mapFn :: (a -> b) -> (c -> a) -> (c -> b)
-- This is just function composition! ;)
```

### Lecture 5 - Data types and type classes

#### Exercise 1

```haskell
data Complex = C Float Float

instance Num Complex where
  (C a b) + (C x y) = C (a + x)    (b + y)
  (C a b) - (C x y) = C (a - x)    (b - y)
  (C a b) * (C x y) = C (a*x-b*y)  (a*y+b*x)
  negate (C a b)    = C (negate a) (negate b)
  abs    (C a b)    = C (a*a+b*b)  0
  fromInteger i     = C (fromInteger i) 0
```

#### Exercise 3

```haskell
data Set a = Set [a]

-- 'xs' is a subset of 'ys' if every element
-- of 'xs' is an element of 'ys'
subset :: Eq a => Set a -> Set a -> Bool
subset (Set xs) (Set ys) = all (\x -> elem x ys) xs

instance Eq a => Eq (Set a) where
  (==) = subset
```

We need to make `Set a` its own data type because otherwise we would get two conflicting instances for `[a]`.

#### Exercise 4

```haskell
class Finite a where
  elements :: [a]

instance Finite Bool where
  elements = [False, True]

instance (Finite a, Finite b) => Finite (a, b) where
  elements = [(x, y) | x <- elements, y <- elements]

-- Auxiliary definition for Finite (Set a)
-- Computes all subsets for the given elements,
-- that is, all combinations where each element
-- in the list may or may not appear
allSubsets :: [a] -> [Set a]
allSubsets []     = [[]]
allSubsets (v:vs) = let ss = allSubsets vs
                     in ss ++ [v:s | s <- ss]

instance Finite a => Finite (Set a) where
  elements = allSubsets elements

-- Auxiliary definition for Finite (a -> b)
-- Computes all key-value pairs from two lists,
-- the first one gives the keys and the second
-- one gives the possible values
allKVPairs :: [k] -> [v] -> [[(k, v)]]
allKVPairs []     _  = [[]]
allKVPairs (k:ks) vs = [(k,v):kvs
                         | kvs <- allKVPairs ks vs
                         , v   <- vs]

instance (Finite a, Finite b, Eq a) => Finite (a -> b) where
  elements = [\k -> fromJust (lookup k kv)
               | kv <- allKVPairs elements elements]
```

### Lecture 6 - Data structures

#### Exercise 1

```haskell
findTree :: (a -> Bool) -> Tree a -> Maybe a
findTree _ Leaf = Nothing
findTree p (Node x l r)
  | p x         = Just x
  | otherwise   = findTree p l <|> findTree p r

(<|>) :: Maybe a -> Maybe a -> Maybe a
Nothing <|> y = y
Just x  <|> _ = Just x
```

#### Exercise 2

```haskell
traverseTree :: (a -> [a] -> [a]) -> Tree a -> [a]
traverseTree _       Leaf         = []
traverseTree combine (Node x l r) =
  combine x (traverseTree l) (traverseTree r)

preOrderTraversal :: Tree a -> [a]
preOrderTraversal = traverseTree (\x l r -> x : (l ++ r))

inOrderTraversal :: Tree a -> [a]
inOrderTraversal = traverseTree (\x l r -> l ++ (x : r))
```

#### Exercise 7

```haskell
paths :: Tree a -> [[a]]
paths Leaf         = [[]]
paths (Node x l r) = map (x:) (paths l) ++ map (x:) (paths r)
```

### Lecture 7 - Case studies

Here is the definition of `Prop` we use in the slides:

```haskell
data Prop = Basic Bool | Var Char
          | Not Prop
          | Prop :/\: Prop | Prop :\/: Prop | Prop :=>: Prop
```

#### Exercise 1

```haskell
printProp :: Prop -> String
printProp (Basic b) = show b
printProp (Var v) = [v]
printProp (Not p)
  = "not " ++ parensPrintProp p
printProp (p1 :/\: p2) =
  = parensPrintProp p1 ++ " /\ " ++ parensPrintProp p2
printProp (p1 :\/: p2) =
  = parensPrintProp p1 ++ " \/ " ++ parensPrintProp p2
printProp (p1 :=>: p2) =
  = parensPrintProp p1 ++ " => " ++ parensPrintProp p2
  where parens s = "(" ++ s ++ ")"
        parensPrintProp p = parens (printProp p)
```

#### Exercise 2

Using the definitions in the slides, we can write it quite concisely. The only thing to change is the check *for every* assignment into a check for *at least one* assignment.

```haskell
-- Using or :: [Bool] -> Bool
satisfiable p = or [tv as p | as <- assigns (vars p)]
-- Using any :: (a -> Bool) -> [a] -> Bool
satisfiable p = any (\as -> tv as p) (assigns (vars p))
```

If we want to refine it, we can use the function `find :: (a -> Bool) -> [a] -> Maybe a` to return the assignment which makes the proposition true.

```haskell
satisfiable :: Prop -> Maybe (Map Char Bool)
satisfiable = find (\as -> tv as p) (assigns (vars p))
```

#### Exercise 3

Our original definition of `ArithExpr` only accounted for binary operations. But factorial is unary, so we need to expand the data type on that respect. Exponentiation is just a new binary operation, which we can put together with `Plus`, `Minus` and so on.

```haskell
data UnArithOp  = Factorial
data BinArithOp = Plus | Minus | Times | Div | Exp

data ArithExpr = Constant Integer
               | Variable Char
               | UnOp  UnArithOp  ArithExpr
               | BinOp BinArithOp ArithExpr ArithExpr

eval :: Map Char Integer -> ArithExpr -> Integer
eval _ (Constant c)  = c
eval m (Variable v)  = fromJust (lookup v m)
eval m (UnOp  o x)   = evalUnOp  o (eval m x)
  where evalUnOp Factorial = \x -> product [1 .. x]
eval m (BinOp o x y) = evalBinOp o (eval m x) (eval m y)
  where evalBinOp Plus  = (+)
        evalBinOp Minus = (-)
        evalBinOp Times = (*)
        evalBinOp Div   = div
        evalBinOp Exp   = (^^)
```

### Lecture 13 - Project management: testing with QuickCheck

#### Exercise 1

```haskell
propFilterNoLonger :: (a −> a) −> [a] −> Bool
propFilterNoLonger p xs = length (filter p xs) <= length xs

propFilterAllSatisfy :: (a −> a) −> [a] −> Bool
propFilterAllSatisfy p xs = all p (filter p xs)

propFilterAllElements :: Eq a => (a −> a) −> [a] −> Bool
propFilterAllElements p xs = all (`elem` xs) (filter p xs)
```

In order to completely characterize the `filter` function you should add two properties:

- That the elements which are left out do not satisfy the property.
- That the result of `filter p xs` is a sublist of `xs`, that is, a copy of `xs` where some elements are removed but the order of the elements is respected.

#### Exercise 2

```haskell
propMapLength :: (a −> b) −> [a] −> Bool
propMapLength f xs = length xs == length (map f xs)

propMapId :: Eq a => [a] -> Bool
propMapId xs = map id xs == xs
```

One property which is difficult to verify is the distributivity over composition, that is, that `map (f . g) = map f . map g`. This is because generating arbitrary functions is a complicated task.

#### Exercise 3

```haskell
propPermsLength :: [Int] −> Bool
propPermsLength xs = length (permutations xs) == factorial (length xs)

isPerm :: [a] −> [a] −> Bool
isPerm xs ys = length xs == length ys && all (`elem` ys) xs

propPermsArePerms :: [Int] −> Bool
propPermsArePerms xs = all (isPerm xs) (permutations xs)
```

An additional property which characterized `permutations` is that given a list without duplicates, the set of permutations does not contain duplicate elements either.


### Lecture 9 - Input and output

#### Exercise 1

```haskell
import System.Random

main :: IO ()
main = do l <- randomRIO (1,  50)
          u <- randomRIO (51, 100)
          guess l u
```

#### Exercise 2

```haskell
sequence []     = return []
sequence (r:rs) = r >>= \x ->
                  sequence rs >>= \xs ->
                  return (x:xs)

sequence_ []     = return ()
sequence_ (r:rs) = r >> sequence_ rs
```

#### Exercise 4

```haskell
do putStr "Give a number:"
   n <- getInt
   forM_ [1 .. 10] $ \m -> do
     putStr (show m)
     putStr " * "
     putStr (show n)
     putStr " = "
     putStrLn (show (m * n))
   putStrLn "Goodbye"
```

#### Exercise 5

```haskell
appendAll :: [FilePath] −> FilePath −> IO ()
appendAll inputs output = do
  contents <- mapM readFile inputs
  writeFile output (concat contents)
```

```haskell
main :: IO ()
main = do out <- askOutputPath
          ins <- askInputPaths
          appendAll ins out

askOutputPath :: IO String
askOutputPath = do putStr "Output path: "
                   line <- getLine
                   case line of
                     "" -> do putStrLn "Invalid path"
                              askOutputPath
                     _  -> return line

askInputPaths :: IO String
askInputPaths = reverse (askInputPaths' [])
  where askInputPaths' acc = do
          putStr "Input path: "
          line <- getLine
          case line of
            "" -> return acc
            x  -> askInputPaths' (x:acc)
```

### Lecture 10 - Laws and induction

Solutions to several of the exercises can be found in Chapter 16 of the [Lecture Notes](http://www.staff.science.uu.nl/~hage0101/FP-elec.pdf).

### Lecture 11 - Lazy evaluation

#### Exercise 1

Given that definition, when the machine computes the result of `intersperse 'a' ('b' : undefined)`, it first checks whether the constructor for the second argument is `[]` or `(:)`. We know that from the code, so nothing additional needs to be forced. However, to decide between the second and third branches it needs to know the next constructor, since it needs to distinguish `x : []` from `x : (y : zs)`. Because at that point the expression is undefined, the result of `intersperse 'a' ('b' : undefined)` is also undefined.

In order to make the definition less strict, we need to pattern match as little as possible. One way to do this is taking the first element as is, and then prepending the separator to the rest:

```haskell
intersperse _ []     = []
intersperse s (x:xs) = x : intersperse' xs
  where intersperse' []     = []
        intersperse' (y:ys) = s : y : intersperse' ys
```

Using this definition, the result of `intersperse` produces two elements before it gets to the undefined part of the list. In other words, the result of the execution is `'b' : 'a' : undefined`.

### Lectures 12 and 13 - Functors, monads, applicatives and traversables

#### Exercise 2

```haskell
tuple :: Monad m => m a -> m b -> m (a, b)
-- Using do notation
tuple x y = do x' <- x
               y' <- y
               return (x', y')
-- Using explicit binds
tuple x y = x >>= \x' -> x >>= \y' -> return (x', y')
-- Use applicative operators
tuple x y = (,) <$> x <*> y
-- Even shorter
tuple = liftM2 (,)
```

In the `Maybe` case this function returns a tuple in a `Just` only if both elements are a `Just`, or a `Nothing` if any of them (or both) holds no value. In code, that description looks like:

```haskell
case x y of
  (Just x', Just y') -> Just (x', y')
  (_,       _)       -> Nothing
```

#### Exercise 3

In this solution I am assuming that you implement the `State` monad using the following data type:

```haskell
data State s a = S (s -> (a, s))
```

The definitions of `get`, `put`, and `modify` are:

```haskell
get :: State s s
get = S (\s -> (s, s))

put :: s -> State s ()
put newState = S (\_ -> ((), newState))
-- or using modify
put newState = modify (\_ -> newState)
-- other possibility
put newState = modify (const newState)

modify :: (s -> s) -> State s ()
modify f = S (\s -> ((), f s))
-- or using get and put
modify f = do s <- get
              put (f s)
```

#### Exercise 4

The general type for `sequence` is `Monad m => [m a] -> m [a]`. If we concretize the type to `Maybe`, we get to `[Maybe a] -> Maybe [a]`. This already tells us that the final result of a `sequence` is going to be a list which might be absent.

Let us now look at the definition of `sequence`:

```haskell
sequence []     = return []
sequence (r:rs) = do x  <- r
                     xs <- sequence rs
                     return (r:rs)
```

At this point we need to remember what the monad instance for `Maybe` does. If at some point the right-hand side of a `<-` is `Nothing`, the entire result becomes `Nothing`. Following this reasoning, `sequence ms` returns a `Just` only when *every* element in the list `ms` is a `Just`, and `Nothing` otherwise.

#### Exercise 5

```haskell
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ v []     = return v
foldM f v (x:xs) = do w <- f v x
                      foldM f w xs
```

#### Exercise 7

```haskell
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)

instance Monad Expr where
  -- return :: a -> Expr a
  return x = Var x

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  (Var a)   >>= f = f a
  (Val n)   >>= f = return (Val n)
  (Add x y) >>= f = Add <$> (x >>= f) <*> (y >>= f)
```

This binding operation performs *substitution*. That is, if we have an expression `x` and a function `f` which maps those variables to further expressions, then `x >>= f` is the result of applying that map to `x`.
