---
title: Pen-and-paper - Exercises
---

These exercises are taken from the [Lecture Notes](http://www.cs.uu.nl/people/jur/FP-elec.pdf) from previous years. The exercises at the end of each chapter of *Programming in Haskell* by Graham Hutton are also suggested.

### Lectures 1 and 2 - Functions and types

1. Write two version of a function `noOfSol` that, for some `a`, `b`, and `c`, determines the number of solutions of the equation `ax² + bx + c = 0`:
    * with case distinction
    * by combining standard functions

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

10. Define a function that determines the "run-length encoding" of a list: `[1, 2, 2, 3, 2, 4]` is mapped to `[(1, 1),(2, 2),(1, 3),(1, 2),(1, 4)]`.

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

3. Define a type `Set a` which consists of elements of type `a`. Define a function `subset :: Set a -> Set a -> Bool` which checks whether all the elements in the first set also belong to the second. Use this to define an `Eq` instance for `Set a`.

    - Why do we have to define `Set a` as its own data type, instead of an alias over `[a]`?

4. Define a class `Finite`. This class has only one method: the list of all the elements of that type. The idea is that such list is finite, hence the name. Define the following instances for `Finite`:
    - `Bool`.
    - `Char`.
    - `(a, b)` for finite `a` and `b`.
    - `Set a`, as defined in the previous exercise, when `a` is finite.
    - `a -> b` whenever `a` and `b` are finite and `a` supports equality. Use this to make `a -> b` an instance of `Eq`.

### Lecture 6 - Data structures

1. Write a "search tree version" of the function `find`, in the same sense that `elemTree` is a "search tree version" of `elem`. Also write down the type.

2. Write functions that return all values of type `a` in a tree of type `Tree a` in depth-first (pre-order), depth-first (in-order), and breadth-first order.

3. Write a function `showTree` that gives a nice representation as a `String` for a given tree\. Every leaf should be placed on a different line (separated by `"\n"`). Leaves that are deeper in the tree should be further to the right than leaves higher in the tree.

4. Write a function `mapTree` and a function `foldTree` that work on a `Tree`, analoguous to `map` and `foldr` on lists. Also give the type of these functions.

5. Write a function `height`, that computes the amount of levels in a `Tree`. Give a definition using recursion, and a different defition using `mapTree` and `foldTree`.

6. Suppose that a tree `t` has height `n`. What is the minimal and maximal amount of leaves that `t` could have?

7. Write a function that computes all paths of type `[a]` from the root up to a leaf for a tree of type `Tree a`.

8. Write a function that computes a list of nodes that occur on one of the longest paths from the root up to a leaf. Try to keep the solution linear with respect to the size of the tree.

### Lecture 7 - Case studies