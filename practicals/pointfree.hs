-- Convert the following expressions into point-free style
\x y -> x y

\x -> f (g x)

\xs -> filter (\x -> not (p x)) xs

\x y -> x + 1

\v1 v2 -> sum (zipWith (*) v1 v2)

\x y z -> f (g x y z)

\x y z -> f (g x y) z

\x y z -> f z (g x y)

\(a,b) -> (b,a)

\ a b -> b a

\ x -> x * x
    
\a b -> a:b:[]

\x -> x+x+x

\a b -> Nothing

\(a,b) -> (f a, g b)

\f g h x -> f x `h` g x

\x y -> x . f . y

\f xs -> xs >>= return . f

\h f g x -> f x `h` g x

\f a b c d -> f b c d a

\a (b,c) -> a c b

\x y -> compare (f x) (f y)