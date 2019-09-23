---
title: Exercises
---

These exercises are taken from the [Lecture Notes](http://www.cs.uu.nl/people/jur/FP-elec.pdf) from previous years. The exercises at the end of each chapter of *Programming in Haskell* by Graham Hutton are also suggested.

If you like a more interactive approach, [Ask-Elle](http://ideas.cs.uu.nl/AskElle/) and [exercism.io](http://exercism.io/languages/haskell) provide Haskell exercises which are automatically corrected.

### [Solutions to selected exercises](solutions.html) - [Exams from previous years](exams.html)

### Schedule

<table class="table table-stripped" style="font-size: 15px;">
    <tr>
    <th>Date</th>
    <th>Exercises</th>
</tr>
<tr>
    <td>Tue 10 Sep</td>
    <td>Lectures 1 and 2</td>
</tr>
<tr>
    <td>Tue 17 Sep</td>
    <td>Lectures 3 and 4</td>
</tr>
<tr>
    <td>Tue 24 Sep</td>
    <td>Lectures 5 and 6</td>
</tr>
<tr>
    <td>Tue 1 Oct</td>
    <td>-</td>
</tr>
<tr>
    <td>Tue 8 Oct</td>
    <td>Lectures 7, 8 and 9</td>
</tr>
<tr>
    <td>Tue 15 Oct</td>
    <td>Lecture 11</td>
</tr>
<tr>
    <td>Tue 22 Oct</td>
    <td>Lectures 12 and 13</td>
</tr>
<tr>
    <td>Tue 29 Oct</td>
    <td>Work on anything</td>
</tr>
</table>

### Lectures 1 and 2 - Functions and types

1. Write a function `noOfSol` that, for some `a`, `b`, and `c`, determines the number of solutions of the equation `ax² + bx + c = 0`, using case distinction.

2. What is the type of the following functions? `tail`, `sqrt`, `pi`, `exp`, `(ˆ)`, `(/=)` and `noOfSol`? How can you query the interpreter for the type of an expression and how can you explicitly specify the types of functions in your program?

3. Given the following definitions:

    ```haskell
    thrice x = [x, x, x]

    sums (x : y : ys) = x : sums (x + y : ys)
    sums xs           = xs
    ```

    What does the following expression evaluate to?

    ```haskell
    map thrice (sums [0 .. 4])
    ```

#### Types and inference

In these exercises you should assume the following types:

```haskell
foldr  :: (a -> b -> b) -> b -> [a] -> b
map    :: (a -> b) -> [a] -> [b]
concat :: [[a]] -> [a]
(.)    :: (b -> c) -> (a -> b) -> a -> c
```

1. What is the type of `foldr map`?
    a. `[a] -> [a -> a] -> [a]`
    b. `[a] -> [[a -> a]] -> [a]`
    c. `[a] -> [[a -> a] -> [a]]`
    d. `[[a]] -> [a -> a] -> [a]`

2. What is the type of `map . foldr`?
    a. `(a -> a -> a) -> [a] -> [[a] -> a]`
    b. `(a -> a -> a) -> [b] -> [b -> a]`
    c. `(b -> a -> a) -> [a] -> [[b] -> a]`
    d. `(b -> a -> a) -> [b] -> [[a] -> a]`

3. Which of the following is the type of `concat . concat`?
    a. `[[a]] -> [[a]] -> [[a]]`
    b. `[[a]] -> [[a]] -> [a]`
    c. `[[[a]]] -> [a]`
    d. `[a] -> [[a]] -> [a]`

4. What is the type of `map (map map)`?
    a. `[[a -> b]] -> [[[a] -> [b]]]`
    b. `[a -> b] -> [[[a] -> [b]]]`
    c. `[[a -> b]] -> [[[a -> b]]]`
    d. `[[a -> b] -> [[a] -> [b]]]`

5. Which observation is correct when comparing the types of `(map  map) map` and `map (map map)?`
    a. The type of the first is less polymorphic than the type of the second.
    b. The type of the first is more polymorphic than the type of the second.
    c. The types are the same, since function composition is associative.
    d. One of the expressions does not have any type at all.

### Lecture 3 - Lists and recursion

1. A recursive function is only sensible if the condition is that the value of its parameters becomes simpler in each recursive application is met. Consider the following definition of the `factorial` function:

    ```haskell
    fac n | n == 0 = 1
          | otherwise = n * fac (n − 1)
    ```

    - What happens if you evaluate `fac (−3)`?
    - How can you formulate the condition more precisely?

2. Here is a definition for exponentiation:

    ```haskell
    x ^ 0 = 1
    x ^ n = x * (x ^ (n-1))
    ```
    
    * Give an alternative definition for that treats the two cases where `n` is even and where `n` is uneven separately. You can exploit the fact that `x ^ n = (x ^ (n/2)) ^ 2`
    * Which intermediate results are being computed for the computation of `2 ^ 10` in the old and the new definition?

3. Define a function which returns the last element of a list.

4. Define a function that returns the one but last element of a list.

5. Define an operator `(!!)` which returns the `i`-th element of a list

6. Define a function that determines whether a list is a palindrome, that is, whether the list is equal to its reversal.

7. Define the function `concat :: [[a]] −> [a]` which flattens a list of lists: `concat [[1, 2], [3], [ ], [4, 5]]` evaluates to `[1, 2, 3, 4, 5]`.

8. Define a function `remSuccessiveDuplicates` which removes succesive repeated elements from a list: `[1, 2, 2, 3, 2, 4]` is mapped to `[1, 2, 3, 2, 4]`.

9. Define a function that groups successive duplicate elements in a list into sublists: `[1, 2, 2, 3, 2, 4]` is mapped to `[[1], [2, 2], [3], [2], [4]]`.

10. Define a function that determines the "run-length encoding" of a list: `[1, 2, 2, 3, 2, 4]` is mapped to `[(1, 1),(2, 2),(1, 3),(1, 2),(1, 4)]`. That is, the list is mapped to a list of pairs whose first element says how many times the second component of the pair appears in adjacent positions in the list.
    - Define a function which constructs the original list given its run-length-encoded version.

11.  Verify that the definition of `(++)` indeed maps `[1, 2] ++ []` to `[1, 2]`. Hint: write `[1, 2]` as `1 : (2 : [ ])`.

12. Which of the following expressions returns `True` for all lists `xs`, and which `False`?

    ```haskell
    [[ ]] ++ xs   == xs
    [[ ]] ++ xs   == [xs]
    [[ ]] ++ xs   == [[ ], xs]
    [[ ]] ++ [xs] == [[ ], xs]
    [xs]  ++ [ ]  == [xs]
    [xs]  ++ [xs] == [xs, xs]
    ```

15. Write a function which takes two lists and removes all the elements from the second list from the first list. (This function is defined in `Data.List` as `(\\)`.)

16. We can represent a matrix as a list of lists of the same length. Write a function `transpose :: [[a]] -> [[a]]` which maps the `i`-th element of the `j`-th list to the `j`-th element of the `i`-th list. Hint: make use of the function:

    ```haskell
    zipWith op (x:xs) (y:ys) = (x `op` y) : zipWith xs ys
    zipWith op _      _      = [ ]
    ```

17. Implement the function `split` with the following type: `split :: Int -> [a] -> [[a]]`. This function divides the given list in sublists, where the sublists have the given length. Only the last list might be shorter. The function can be used as follows:
  
    ```
    > split 3 [1..11]
    [[1,2,3],[4,5,6],[7,8,9],[10,11]]
    ```

18. Write a function `gaps` that gives all the possibilities to take out one element from a list. For example:
    
    ```
    gaps [1,2,3,4,5] = [[2,3,4,5], [1,3,4,5], [1,2,4,5], [1,2,3,5], [1,2,3,4]]
    ```

### Lecture 4 - Higher-order functions

1. Give examples for functions with the following types:
    
    ```haskell
    (Float −> Float) −> Float
    Float −> (Float −> Float)
    (Float −> Float) −> (Float −> Float)
    ```

2. Define the function `concat` using `foldr`.

3. Define the functions `inits` and `tails` using `foldr`.

4. The function `filter` can be defined in terms of `concat` and `map`:

    ```haskell
    filter p = concat . map box
      where box x = _
    ```

    Complete the definition of `box`.

5. Function composition first applies the latter of the supplied functions to the argument, the former thereafter. Write a function before that can be used to rewrite `f . g . h` to ``h `before` g `before` f``. What can you say about associativity of `(.)` and `before`?

6. We have seen that `[...]` is a type function that maps types to types. Similarly because `−>` is a type constructor mapping two types to a type, for some `c` also `c −>` is a type function mapping a type `a` to `c −> a`. Rewrite the type of `map` by substituting the type function `[...]` by `c −>`. Can you derive an implementation from the resulting type?

7. The function `map` can be applied on functions. Its result is a function as well (with a different type). There are no restrictions on the function type on which `map` is applied, it might even be applied to `map` itself! What is the type of the expression `map map`?

### Lecture 5 - Data types and type classes

1. Write a data type `Complex` to represent complex numbers of the form `a + b*i`. Write its `Num` instance.

2. Give a direct definition of the `<` operator on lists. This definition should not use operators like `<=` for lists. (When trying out this definition using `ghci`, do not use the `<` symbol, since it is already defined in the `Prelude`).

3. Define a type `Set a` which consists of elements of type `a`. Define a function `subset :: Eq a => Set a -> Set a -> Bool` which checks whether all the elements in the first set also belong to the second. Use this to define an `Eq` instance for `Set a`.

    - Why do we have to define `Set a` as its own data type, instead of an alias over `[a]`?

4. Define a class `Finite`. This class has only one method: the list of all the elements of that type. The idea is that such list is finite, hence the name. Define the following instances for `Finite`:
    - `Bool`.
    - `Char`.
    - `(a, b)` for finite `a` and `b`.
    - `Set a`, as defined in the previous exercise, when `a` is finite.
    - `a -> b` whenever `a` and `b` are finite and `a` supports equality. Use this to make `a -> b` an instance of `Eq`.

### Lecture 6 - Data structures

1. Write the function `elemTree` that tests if an element `x` occurs
   in a binary search tree of type `Tree a`.

2. Write functions that return all values of type `a` in a tree of type `Tree a` in depth-first (pre-order), depth-first (in-order), and breadth-first order.

3. Write a function `showTree` that gives a nice representation as a `String` for a given tree. Every leaf should be placed on a different line (separated by `"\n"`). Leaves that are deeper in the tree should be further to the right than leaves higher in the tree.

4. Write a function `mapTree` and a function `foldTree` that work on a `Tree`, analoguous to `map` and `foldr` on lists. Also give the type of these functions.

5. Write a function `height`, that computes the amount of levels in a `Tree`. Give a definition using recursion, and a different defition using `mapTree` and `foldTree`.

6. Suppose that a tree `t` has height `n`. What is the minimal and maximal amount of leaves that `t` could have?

7. Write a function that computes all paths of type `[a]` from the root up to a leaf for a tree of type `Tree a`.

8. Write a function that computes a list of nodes that occur on one of the longest paths from the root up to a leaf. Try to keep the solution linear with respect to the size of the tree.

### Lecture 7 - Case studies

1. Define a function `printProp :: Prop -> String` which turns the proposition into a printable `String`.
    - Define a new `printProp' :: Prop -> String` which uses as few parentheses as possible. For example, `Var 'A' :\/: (Var 'B' :\/: Var 'C')` should be printed as `A \/ B \/ C`. Hint: define an auxiliary function `printProp'' :: Prop -> (String, LogicalOp)` which remembers the top symbol of the formula.

2. Define a function `satisfiable :: Prop -> Bool` which returns `True` is the proposition is satisfiable, that is, if there is at least one assignment of truth values to variables which make the proposition true.
    - Refine the function to return the assignment which makes the proposition satisfiable. Which should be the type given to such a function?

3. Extend the definition of `ArithExpr` and `RPN` to include exponentiation and factorial functions. How should the evaluation functions change to support them?

4. Refine the solution for Countdown to return the expression which gives the nearest result to the target, instead of only returning those which give the exact answer.

5. Define a function `rpnToExpr :: RPN -> ArithExpr` which converts an expression in Reverse Polish Notation to an expression in usual infix notation.

### Lecture 8 - Project management: testing with QuickCheck

For the exercises below you may want to consult the functions provided by the [QuickCheck library](hackage.haskell.org/package/QuickCheck-2.4.2/docs/Test-QuickCheck.html), in particular functions such as `choose`, `sized`, `elements` and `frequency`. We encourage experimenting with your code in an interpreter session. To be able to experiment with QuickCheck, the first two exercises work better if you can `show` functions. For that you can add the following instance definition to your code:

```haskell
instance (Enum a, Bounded a, Show  a) => Show (a −> Bool) where
  show f = intercalate "\n" (map (\x −> "f " ++ show x ++ " = " ++ show (f x)) [minBound .. maxBound])
```

Also when you run your tests, you sometimes need to specialize the types a bit. For example, the following code calls all kinds of test functions that the exercises below (except for 4) expect you to come up with.

```haskell
runTests :: IO ()
runTests = do
  putStrLn "\nExercise 14.1"
  quickCheck (propFilterNoLonger      :: (Bool −> Bool) −> [Bool] −> Bool)
  quickCheck (propFilterAllSatisfy    :: (Bool −> Bool) −> [Bool] −> Bool)
  quickCheck (propFilterAllElements   :: (Bool −> Bool) −> [Bool] −> Bool)
  quickCheck (propFilterCorrect       :: (Bool −> Bool) −> [Bool] −> Bool)
  putStrLn "\nExercise 14.2"
  quickCheck (propMapLength :: (Bool −> Bool) −> [Bool] −> Bool)
  putStrLn "\nExercise 14.3"
  quickCheck $ once (propPermsLength   :: [Int] −> Bool)
  quickCheck $ once (propPermsArePerms :: [Int] −> Bool)
  quickCheck $ once (propPermsCorrect  :: [Int] −> Bool)
  putStrLn "\nExercise 14.5"
  quickCheck (forAll genBSTI isSearchTree)    -- Use forAll to use custom generator
  quickCheck (forAll genBSTI propInsertIsTree)
  quickCheck (forAll genBSTI propInsertIsTreeWrong)
```

1. Consider the ubiquitous `filter` function. There are many properties that you can formulate for the input-output behaviour of `filter`.
    - Formulate the QuickCheck property that the result list cannot be longer than the input.
    - Formulate the QuickCheck property that all elements in the result list satisfy the given property.
    - Formulate the QuickCheck property that all elements in the result list are present in the input list.
    - Formulate a set of QuickCheck properties to completely characterize the `filter` function (you may choose  also from among the three you have just implemented). Make sure to remove properties that are implied by (a subset of) the other properties.

2. Try to come up with a number of QuickCheck-verifiable properties for the `map` function, and implement these. Are there any properties of `map` that are awkward to verify?

3. Consider the function `permutations` from the `Data.List` library, which computes all the possible permutations of elements in a list. We shall be writing QuickCheck tests to verify that this function.
    - Write a QuickCheck property that checks that the correct number of permutations is generated.
    - Write a function `isPerm :: [a] −> [a] −> Bool` that  verifies that the two argument lists are permutations of each other.
    - Write the QuickCheck property that every list in the output of `permutations` is a permutation of the input.
    - Formulate a set of properties to completely characterize the `permutations` function (you may choose also from among the ones you have just implemented). Make sure to remove properties that are implied by (a subset of) the other properties. Implement the properties that you still need as QuickCheck properties.

4. Do something similar for the function `inits` defined in Lecture 3.

5. Consider the following datatype definition for binary trees that we shall want to use to implement binary search trees:

    ```haskell
    data Tree a = Branch a (Tree a) (Tree a) | Leaf
    ```

    Write a function `isSearchTree :: Tree a −> Bool` that verifies that its argument is a binary search tree. Then test the property that given a binary search tree `t`, inserting a value into the tree results in yet another binary search tree. The code for inserting a new value into the tree is:

    ```haskell
    insertTree :: Ord a => a −> Tree a −> Tree a
    insertTree e Leaf = Branch e Leaf Leaf
    insertTree e (Branch x li re)
      | e <= x = Branch x (insertTree e li) re
      | e >  x = Branch x li (insertTree e re)
    ```

    Experiment with mutating the implementation of `insertTree` to find out whether your property can in fact discover that the mutated implementation no longer maps binary search trees to binary search trees.

### Lecture 9 - Input and output

1. Extend the "guess a number" game to generate the bounds randomly.

2. Write `sequence` and `sequence_` using `(>>=)` instead of `do`-notation.

3. Write a function which prompts for a filename, reads the file with this name, splits the file into a number of lines, splits each line in a number of words separated by `' '` (a space), and prints the total number of lines and words.

4. Given the function `getInt :: IO Int`, which reads an integer value from standard input, write a program that results in the following input/output behaviour (the `3` has been typed in by the user):

    ```
    Give a number: 3
    1 * 3 = 3
    2 * 3 = 6
    3 * 3 = 9
    ...
    10 * 3 = 30
    Goodbye
    ```
 
    Try to use `sequence_`, `mapM_` or `forM_`.

5. Write a function of type `[FilePath] −> FilePath −> IO ()` which concatenates a list of files to a specific target file: the first parameter is a list of filenames and the second parameter the name of the target file. Do not use the function `appendFile`.

    - Write a program that first asks for the name of the target file, and then continues asking for names of files to be appended to that file until an empty line is entered. Note that the target files may be one of the source files!
    - If we know that none of the source files equals the target file we may do a bit better using the function [`appendFile` from `System.IO`](https://hackage.haskell.org/package/base/docs/System-IO.html#v:appendFile). Change the function you have written above using this function. What are the advantages and disadvantages of this approach?

### Lecture 11 - Laws and induction

1. Finish the proof of `reverse . reverse = id`.

2. Prove the following laws about list functions.
    - Hint: if you get stuck, the proofs can be found in Chapter 16 of the [Lecture Notes](http://www.staff.science.uu.nl/~hage0101/FP-elec.pdf).

    ```haskell
    -- Laws about list length
    length . map f    = length
    length (xs ++ ys) = length xs + length ys
    length . concat   = sum . map length

    -- Laws about sum
    sum (xs ++ ys) = sum xs ++ sum ys
    sum . concat   = sum . map sum

    -- Laws about map
    map f . concat = concat . map (map f)  -- Hard!
    ```

3. Prove that `sum (map (1+) xs) = length xs + sum xs` for all lists `xs`.
    a. State a similar law for a linear function `sum (map ((k+) . (n*)) xs) = ??`.
    b. Prove the law from (a).
    c. Which laws from the previous exercise can be deduced from the general law?

4. Prove the following law: if `op` is an associative operator and `e` its neutral element, then

    ```haskell
    foldr op e . concat = foldt op e . map (foldr op e)
    ```

5. Find a function `g` and a value `e` such that

    ```haskell
    map f = foldr g e
    ```

    Prove the equality between both sides.

6. Prove that addition is commutative, that is, `add n m = add m n`.
    - Hint: you might need as lemmas that `add n Zero = n` and `add n (Succ m) = Succ (add n m)`.

7. Prove that multiplication is commutative, that is, `mult n m = mul m n`.
    - Hint: you need lemmas similar to the previous exercise.

8. Prove that for all trees `size t = length (enumInfix t)`.
    - Hint: you might needs some of the laws in exercise 2 as lemmas.

9. Prove that `length . catMaybes = length . filter isJust`, where

    ```haskell
    catMaybes :: [Maybe a] -> [a]
    catMaybes []             = []
    catMaybes (Nothing : xs) = catMaybes xs
    catMaybes (Just x  : xs) = x : catMaybes xs

    isJust :: Maybe a -> Bool
    isJust (Just _) = True
    isJust Nothing  = False
    ```

    - Hint: proceed by induction on the list, and in the `z:zs` case distinguish between `z` being `Nothing` or `Just w`.

### Lecture 12 - Lazy evaluation

1. In an older version of the base library the function `intersperse`, which places an element between all elements of a list, was defined as:

    ```haskell
    intersperse _ []       = []
    intersperse _ [x]      = [x]
    intersperse e (x:y:ys) = x : e : intersperse e (y:ys)
    ```

    - What would you expect the result of the expression `intersperse 'a' ('b' : undefined)` to be?
    - Can you give a definition of `intersperse` which is less strict?

2. Given the data type of binary trees:

    ```haskell
    data Tree a = Leaf a | Node (Tree a) (Tree a)
    ```

    we define the function `tja`:

    ```haskell
    tja t = let tja' (Leaf a)   n ls = (0, if n == 0 then a : ls else ls)
                tja' (Node l r) n ls = let (lm, ll) = tja' l (n-1) rl
                                           (rm, rl) = tja' r (n-1) ls
                                        in ((lm `min` rm) + 1, ll)
                (m, r) = tja' t m []
             in r
    ```

    If this code computes something explain what it computes, maybe with the aid of a small example. If it does not compute anything, explain why this is the case.

### Lectures 10 and 13 - Functors, monads, applicatives and traversables

1. Show that the definition of the arithmetic evaluator using `next` in Lecture 13 is the same as the one using nested `case` clauses by expanding the definition of the former.

2. Define a function `tuple :: Monad m => m a -> m b -> m (a, b)` using explicit `(>>=)`, `do`-notation and applicative operators.
    - What does the function do in the `Maybe` case?

3. Define the following set of actions for `State s a`:
    - `get :: State s s` obtains the current value of the state.
    - `modify :: (s -> s) -> State s ()` updates the current state using the given function.
    - `put :: s -> State s ()` overwrites the current state with the given value.
    
    Using those primitive operations:
    * Define `modify` using `get` and `put`.
    * Define `put` using `modify`.
    * Update the definition of `relabel` in the slides using these actions.

4. Explain the behavior of `sequence` for the `Maybe` monad.

5. Define a monadic generalisation of `foldl`:

    ```haskell
    foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
    ```

6. Show that the `Maybe` monad satisfies the monad laws.

7. Given the type:

    ```haskell
    data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
    ```

    of expressions built from variables of type `a`, show that this type is monadic by completing the following declaration:

    ```haskell
    instance Monad Expr where
      -- return :: a -> Expr a
      return x = ...

      -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
      (Var a)   >>= f = ...
      (Val n)   >>= f = ...
      (Add x y) >>= f = ...
    ```

    With the aid of an example, explain what the `(>>=)` operator for this type does.
