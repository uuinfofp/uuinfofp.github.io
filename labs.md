---
title: Labs
---

### Assignments

### Coding style

The most important question you should ask yourself to judge your own coding style is to ask yourself the question **"If another proficient Haskell programmer reads my code, would this be the most readable code I could have written?"** Where *readable* can be understood as the time it would take him or her to understand what your code tries to accomplish and how it accomplishes this.

* By far the most important factor that affects readability (and thus your grade) is proper use and reuse of existing library functions and syntactic sugar.
* Use the proper idioms (e.g., use `Nothing` instead `(-1)` to indicate failure of a function.)
* Another factor impacting readability is proper indentation of your code, choice of variable and function names, and commenting your code.

To give a concrete example: can you figure out what the following function does?

```haskell
f [a] = h a  -- Base case: call the helper function h on the element in a singleton list
    where h [] = 0  -- Return zero for the empty list
          h (ss:s) = 1 + h s      -- Add one to the recursive call of h.
f (b:a) = if g b > f a then g b else f a      -- If g b is greater than g a then we return g b, otherwise f a
    where g f = foldr (\f g -> g + 1) 0 f   -- Use foldr and a lambda to recurse over f
```

And what about this one?

```haskell
-- Find the length of the longest list in a non-empty list of lists.
lengthOfLongestList :: [[a]] -> Int
lengthOfLongestList = maximum . map length
```

They are equivalent, but it's much easier to figure out what the second one does due to proper coding style. The first definition would get you zero points for style, the second definition all of them.


#### Use and reuse

The most important factor that affects readability is proper use and reuse of existing library functions and syntactic sugar. Some important cases:

* If you want to apply a function uniformly to all the elements in a list then use map instead of writing out the recursion yourself. Instead of writing
 
    ```haskell
    addOne :: [Int] -> [Int]
    addOne []     = []
    addOne (x:xs) = x + 1 : addOne xs
    ```

    write

    ```haskell
    addOne :: [Int] -> [Int]
    addOne = map (+1)
    ```

    All proficient Haskell programmers know that map applies a function uniformly to all elements of a list, while it takes them just a bit longer to figure out this is what is happening if you write out the recursion explicitly.

* If you can write a function or argument to a higher-order function as the composition of existing functions from the standard library, then it is probably a good thing to do so. Write

    ```haskell
    f = filter (odd . someFunction)
    ```

    instead of

    ```haskell
    f xs = filter g xs
       where g x = someFunction x `mod` 2 == 1
    ```

* On the other, it is also possible to take this too far. Most people will probably consider

    ```haskell
    f = map (\x -> 3 * x + 1)
    ```

    to be more readable than
    
    ```haskell
    f = map ((+1) . (*3))
    ```

* Many complicated functions manipulating lists can be written elegantly using list comprehensions.
* Using guards is often preferable over `if`-`then`-`else` or `case`-expressions. Sometimes even if this requires introducing a helper function.

#### Indentation

* The most important aspect about indentation are probably that it is used consistently.
* Try not to make your lines longer than 80 characters. A lot of people still prefer to print out source code, have two editors open side-by-side on their screen, or simply use a large font size on a small screen.
* Try to align matching pieces of code. Instead of:

    ```haskell
    f [] = []
    f (x:xs) | x < 0 = x+x : f (filter odd xs)
             | even x = compute x : f xs
             | otherwise = 2*x+1 : f xs
    ```

    write:

    ```haskell
    f []     = []
    f (x:xs) | x < 0     = x + x     : filter odd (f xs)
             | even x    = compute x :             f xs
             | otherwise = 3 * x - 1 :             f xs
    ```

#### Naming conventions

* Give all your top-level functions descriptive names. Ideally it should be possible to correctly guess what the function does by only knowing its type and name.
* When naming parameters the situation depends on the type of the parameter and what the function does with it:
    - If the function only takes one `Integer` parameter then a short name like `i`, `k` or `n` usually suffices. Example:
        
        ```haskell
        fac :: Integer -> Integer
        fac n = product [1..n]
        ```

    - If the function takes a polymorphic list as an argument or does not inspect the elements in the lists then a short name ending in -s, such as `xs`, `ys` or `zs`, usually suffices. Example:

        ```haskell
        map _ []     = []
        map f (x:xs) = f x : map f xs
        ```

    - If the function does inspect the elements in the list then a more descriptive name might be in order.
    - If the function takes one or two other other functions as parameters then the names `f`, `g`, ... are used. However, if the function parameter is of type `a -> Bool` then the name `p` (from "predicate") is often used. Example:

        ```haskell
        until :: (a -> Bool) -> (a -> a) -> a -> a
        until p f x | p x       = x
                    | otherwise = until p f (f x)
        ```

    - When applying a higher-order function to a list and you need to use a lambda to pass as an argument then name the variable bound by the lambda such that it is easy to figure out to which lists the function gets applied. For example, write:
        
        ```haskell
        f xs ys = xs ++ map (\y -> 3 * y + 1) ys
        ```

        instead of:

        ```haskell
        f xs ys = xs ++ map (\x -> 3 * x + 1) ys
        ```

#### Comments

* Document your functions by writing a short descriptive summary above your code on what the function does (not necessarily how it manages to do this) and what the side conditions are assumed to be satisfied when the function is called. Some good examples can be found at the [`Data.List` module](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-List.html). For example, the documentation for `head`: "Extract the first element of a list, which must be non-empty."
* If you find that you cannot write your code any simpler than you have done so, and you still think understanding it takes effort, then add comments to explain how something has been implemented. An example might be that you chose a particular solution for its efficiency over a clearer solution, and you would like to explain why you chose that solution, and how it works.

#### Tools

There are two important tools than can give you feedback on your coding style. The first is HLint, a tool designed by Neil Mitchell specifically for giving feedback on your coding style. The second is `ghc`, the Haskell compiler itself. When you submit your assignment to DOMjudge both tools will be run on your program. You can view the results of these tools by clicking on your assignment in the list of submission you made on in DOMjudge.

You can install HLint by opening a command prompt and typing:

```
cabal update && cabal install hlint
```

After HLint has been installed you can run it on your source code (which we'll assume you have named `Assignment.hs`) by typing:

```
hlint Assignment.hs
```

HLint will then output a list of suggestions for improving your coding style. Most, although not all, of these suggestions will help to improve your coding style and thus your grade. Of course, computer programs sometimes mistake about subjective issues like coding style, so always use your common sense as well. For example, hlint will always suggest you change `map f (map g)` xs into `map (f . g) xs` (this is called map fusion). If `f` and `g` are simple expressions this will generally be an improvement, but if they are already quite complex, this may not help to improve readability.
