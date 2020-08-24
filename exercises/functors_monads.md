--
title: Functors, monads, applicatives and traversables
---

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
