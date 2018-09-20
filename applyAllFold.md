---
title: applyAll as a fold
---

In the lectures we have seen two variants of `applyAll`, which we would like to express as a fold. Here is a step-by-step explanation on how to reach them.

As a remainder, here are the definitions of `foldl` and `foldr`:

```haskell
foldl f v []     = v                    -- Equation (L1)
foldl f v (g:gs) = foldl f (f v g) gs   -- Equation (L2)

foldr f v []     = v                    -- Equation (R1)
foldr f v (g:gs) = f g (foldr f v gs)   -- Equation (R2)
```

#### Version with explicit application

The first version of `applyAll` we derived in the lectures is:

```haskell
applyAll []     x = x                  -- Equation (A1)
applyAll (g:gs) x = applyAll gs (g x)  -- Equation (A2)
```

From equations (L1) and (A1) we derive:

```haskell
  applyAll [] x = x
= foldl f v []  = v
```

Thus, the initial value `v` must coincide with `x`.

Now, from equations (L2) and (A2) applied to a list of the form `g:gs`:

```haskell
  applyAll (g:gs) x = applyAll gs (g x)
= foldl f x (g:gs)  = foldl f (f x g) gs
```

Here is the trick: *assume* that `applyAll gs t = foldl f t gs` for any `t` (why this is a sensible thing to do is explained in Lecture 10, *Laws and induction*). If that is the case, we just need to find an `f` such that:

```haskell
f x g = g x
-- or written otherwise
f     = \x g -> g x
```

Now we have the two ingredients to reach the final conclusion:

```haskell
applyAll gs x = foldl (\x g -> g x) x gs
```

### Point-free version

The other version we discussed in the lectures builds the `applyAll` by repeated composition:

```haskell
applyAll []     = id               -- Equation (B1)
applyAll (g:gs) = applyAll gs . g  -- Equation (B2)
```

Let us start again by comparing the empty list case:

```haskell
  applyAll []  = id
= foldl f x [] = x
```

Thus, the initial value in this case is the function `id`. If this doesn't make sense to you, remember that now we are returning *a function* as result, and `id` is one such function.

Now to the recursive case. Let us write the equations for a list of the form `g:gs`:

```haskell
  applyAll (g:gs)  = applyAll gs . g
= foldl f v (g:gs) = foldl f (f v g) gs
```

Oh, the do not match! The problem here is that a left-fold *accumulates* its value on a recursive call. But in this case we use the *result* of the recursive call `applyAll gs` to build a larger one. This means we need to look at `foldr` instead.

The calculation of `x` is the same as for `foldl`. So let us focus on the recursive case:

```haskell
  applyAll (g:gs)  = applyAll gs . g = (.) (applyAll gs) g
= foldr f v (g:gs) = f g (foldr f v gs)
```

As previously, let us assume that `applyAll = foldr f v gs`. Then we need to find a function such that:

```haskell
f g x = x . g
```

Here you are! We can just take `f = \g x -> x . g`, or simplify it to `flip (.)`, as done in the slides.

```haskell
applyAll gs = foldr (\g x -> x . g) id
applyAll gs = foldr (flip (.)) id
```