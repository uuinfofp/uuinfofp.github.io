---
title: Laws and Induction
---

1. Finish the proof of `reverse . reverse = id`.

2. Prove the following laws about list functions.
    - Hint: if you get stuck, the proofs can be found in Chapter 16 of the [Lecture Notes](http://www.staff.science.uu.nl/~hage0101/FP-elec.pdf).

    ```haskell
    -- Laws about list length
    length . map f    = length
    length (xs ++ ys) = length xs + length ys
    length . concat   = sum . map length

    -- Laws about sum
    sum (xs ++ ys) = sum xs + sum ys
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
