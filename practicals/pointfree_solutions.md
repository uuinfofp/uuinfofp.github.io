---
title: Solutions to point-free style exercises
---

```haskell
\x y -> x y
-- id

\x y -> x + 1
-- const . (1 +)

\v1 v2 -> sum (zipWith (*) v1 v2)
-- (sum .) . zipWith (*)

\x y z -> f (g x y z)
-- ((f .) .) . g

\x y z -> f (g x y) z
-- (f .) . g

\x y z -> f z (g x y)
-- (flip f .) . g

\(a,b) -> (b,a)
-- uncurry (flip (,))

\ a b -> b a
-- flip id

\ x -> x * x
-- join (*)
    
\a b -> a:b:[]
-- (. return) . (:)

\x -> x+x+x
-- (+) =<< join (+)

\a b -> Nothing
-- const (const Nothing)

\(a,b) -> (f a, g b)
-- f *** g

\f g h x -> f x `h` g x
-- flip . (ap .) . flip (.)

\x y -> x . f . y
--- (. (f .)) . (.)

\f xs -> xs >>= return . f
-- fmap

\h f g x -> f x `h` g x
-- liftM2

\f a b c d -> f b c d a
-- flip . ((flip . (flip .)) .)

\a (b,c) -> a c b
-- (`ap` snd) . (. fst) . flip

\x y -> compare (f x) (f y)
-- ((. f) . compare .)
```