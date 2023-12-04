import System.Random (randomRIO)

-- 1
-- a.
{-
By function extensionality, it is enough to prove that 
curry (f.swap) b a = flip (curry f) b a

curry (f.swap) b a
= g
(f.swap) (b, a)
= h
f (swap (b, a))
= e
f (a, b)
= g
(curry f) a b
= f
flip (curry f) b a

This proves the claim.
-}

-- b.
{-
We distinguish the cases of 
xs = [] and xs = z:zs

First, consider the case xs = [].

Then,
foldr f e ([] ++ ys)
= c
foldr f e ys 
= a
foldr f (foldr f e ys) []



Second, consider the case xs = z:zs.
Assume the induction hypothesis foldr f e (zs ++ ys) = foldr f (foldr f e ys) zs.
Then,

foldr f e ((z:zs) ++ ys)
= d 
foldr f e (z : (zs ++ ys))
= b 
f z (foldr f e (zs ++ ys))
= IH
f z (foldr f (foldr f e ys) zs)
= b
foldr f (foldr f e ys) (z : zs)

The claim now follows by induction on xs.
-}



-- 2.
-- a.
{-
To explain better *why* these are the correct solutions, 
we write down the entire reduction sequence until we 
reach WHNF. Note that the question only asks you 
to write down the final result (the WHNF):

filter even [1, undefined, 2]
->
filter even [undefined, 2]
->
undefined



tail (filter odd [1, 1, undefined, 2])
->
tail (1 : filter odd [1, undefined, 2])
->
filter odd [1, undefined, 2]
-> 
1 : filter odd [undefined, 2]




foldl (\(a, as) xs -> (length xs + a, xs ++ as) ) (0, []) [[1]]
-> 
foldl (\(a, as) xs -> (length xs + a, xs ++ as) ) ((\(a, as) xs -> (length xs + a, xs ++ as) ) (0, []) [1]) []
->
(\(a, as) xs -> (length xs + a, xs ++ as) ) (0, []) [1]
->
(length [1] + 0, [1] ++ [])



Node (Node Leaf 5 Leaf) (3 + 5) Leaf
is already WHNF (a constructor applied to arguments), so we are done.

-}


-- b.
const' :: a -> b -> a 
const' a b = seq b a




-- 3.
gather :: Int -> (Int -> Int) -> [a] -> [a] 
-- a.
gather size f xs = map (\i -> xs !! (f i)) [0.. size - 1]

-- b. -- This question was phrased in a strange way on the exam, so 
-- we have given point to any solution with the requested type
-- that compiles and computes something somewhat sensible.

-- Some examples of acceptable solutions:
spec_gather :: Eq a => Int -> (Int -> Int) -> [a] -> Bool 
spec_gather size f xs = all (\i -> f i < f (i + 1) && 0 <= f i && f (i + 1) <= length xs) [0 .. size-2] {- checks that f is monotone; this is what we had in mind -}
spec_gather2 size f xs = all (\i -> xs !! (f i) == gather size f xs !! i) [0 .. size-1] {- checks that the implementation of gather is correct; not what we had in mind but it is a complete specification of gather -}
spec_gather3 size f xs = all (\i -> elem (gather size f xs !! i) xs) [0 .. size-1] {- checks that we obtain a subset of the original list; not what we had in mind, but it is a reasonable sanity check -}
spec_gather4 size f xs = sublist gathered xs where 
    gathered = gather size f xs
    sublist [] ys = True 
    sublist (x:xs) [] = False 
    sublist (x:xs) (y:ys) | x == y = sublist xs ys 
                          | otherwise = sublist (x:xs) ys
{- checks that the result is a sublist of the original list; not what we had in mind as it does not use all, but it does answer the question otherwise -}

-- 4.

bernoulli :: Double -> DiscProb Int 
bernoulli p = MkProb [(0, 1.0 - p), (1, p)]

-- ps needs to sum up to 1
categorical :: [Double] -> DiscProb Int
categorical ps = MkProb $ zip [0..] ps

newtype DiscProb a = MkProb {probs :: [(a, Double)]} deriving (Show, Eq)
-- -- a. 
instance Functor DiscProb where 
    fmap f (MkProb aws) = MkProb $ map (\(a, w) -> (f a, w)) aws


-- -- b. 
totalWeight :: DiscProb a -> Double 
totalWeight d = foldr (\(_, w) acc -> w + acc)
                      0
                      (probs d)

-- -- c. 
collect :: Eq a => DiscProb a -> DiscProb a 
collect (MkProb aws) = MkProb (foldr insert [] aws) where 
    insert (a,w) [] = [(a,w)]
    insert (a,w) ((a',w'):rest) | a== a'= (a', w+w'):rest
                                | otherwise = (a',w') : insert (a, w) rest

-- -- d.
select :: [(a, Double)] -> Double -> a
select ((x,p):xps) r
    | r < p = x 
    | otherwise = select xps (r-p)

sample :: DiscProb a -> IO a
sample (MkProb xps) = do 
    r <- randomRIO (0, 1)
    let a = select xps r
    return a

-- -- e.
instance Applicative DiscProb where 
    pure = return 
    mf <*> ma = do 
        f <- mf 
        a <- ma 
        return (f a)

instance Monad DiscProb where 
    return a = MkProb [(a, 1.0)]
    (MkProb aws) >>= f = MkProb [(b, w * w') | (a, w) <- aws, (b, w') <- probs (f a)]

example2 :: DiscProb Int 
example2 =
    bernoulli 0.3 >>= \n -> 
    let m = n * 3 in 
    score (fromIntegral n) >>= \_ ->
    return $ n * m


-- -- f.
score :: Double -> DiscProb () 
score w = MkProb [((), w)]

reweight :: (a -> Double) -> DiscProb a -> DiscProb a
reweight f d = do 
    a <- d 
    score (f a)
    return a


-- 5.
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

exampleTree = Node (Node (Leaf 3) 2 (Leaf 4)) 1 (Node (Leaf 6) 5 (Node (Node (Leaf 9) 8 (Leaf 10)) 7 (Leaf 11)))

-- a.
bfs :: Tree a -> [a]
bfs = concat . levels


-- b.
levels :: Tree a -> [[a]]
levels = go . (:[])
    where 
        go :: [Tree a] -> [[a]]
        go [] = []
        go queue = let (lvl, queue') = foldr extend
                                             ([], [])
                                             queue in 
                      lvl : go queue' 
        extend :: Tree a -> ([a], [Tree a]) -> ([a], [Tree a])
        extend (Leaf x) (lvl, queue') = (x : lvl, queue') 
        extend (Node l x r) (lvl, queue') = (x : lvl, l : r : queue')
