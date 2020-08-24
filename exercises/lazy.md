---
title: Lazy Evaluation
---

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
