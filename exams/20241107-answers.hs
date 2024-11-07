-- Solutions for INFOFP Final 2024-2025

-- Question 1a
data Trie w = Step w [(Char, Trie w)] deriving Show

sumWeights :: Num w => Trie w -> w
sumWeights (Step w subTries) = w + sum (map (sumWeights . snd) subTries)

-- Question 1b
instance Functor Trie where
    fmap = mapTrie

mapTrie :: (a -> b) -> Trie a -> Trie b
mapTrie f (Step w subTries) = Step (f w) (map (\(c, t) -> (c, mapTrie f t)) subTries)

-- Question 1c
normalize :: Trie Float -> Trie Float
normalize trie = let total = sumWeights trie in fmap (/ total) trie

-- Question 1d
flatten :: (Eq w, Num w) => Trie w -> [(String, w)]
flatten = filter f . flatten'
    where
        flatten' (Step w chs) = x : (concat (map g chs))
            where x = ([],w)
        f = (/=0) . snd
        g (c,subTrie) = [(c : s, w') | (s,w') <- flatten' subTrie] 

-- Question 1e
autocomplete :: (Eq w, Num w) => String -> Trie w -> [(String, w)]
autocomplete [] trie = flatten trie
autocomplete (x:xs) (Step _ subTries) = case lookup x subTries of
    Just subTrie -> autocomplete xs subTrie
    Nothing -> []

-- Question 2a
while :: c -> (c -> Bool) -> (c -> c) -> c
while init pred step = if pred init then while (step init) pred step else init

-- Question 2b
iter :: (a -> Either a b) -> a -> b
iter f a = let Right b = while (Left a) isLeft (step f) in b
  where
    isLeft (Right _) = False
    isLeft (Left _) = True
    step f (Left a) = f a

fib :: Int -> Int
fib n = iter fibStep (0, 1, n)
  where
    fibStep (fn, _, 0) = Right fn
    fibStep (fn, fsn, count) = Left (fsn, fn + fsn, count - 1)

-- Question 2c
specIter :: Eq b => (a -> Either a b) -> a -> Bool
specIter f a = iter f a == case f a of
    Left a' -> iter f a'
    Right b -> b

-- Question 3a
-- Proof of flip (.) (flip const g) = id would be provided separately as comments
{-
Claim: flip (.) (flip const g) = id
Hint:  Observe that the type of this equation is (a -> b) -> (a -> b).
Proof:
We use extensional reasoning and introduce arguments f :: (a -> b) and x :: a.

Then, we show that
flip (.) (flip const g) f x
= -- b
(.) f (flip const g) x
= -- c
f (flip const g x)
= -- b
f (const x g)
= -- a
f x
= -- d
id f x

-}


-- Question 3b

-- Claim: mapTree id t = t for all trees t
-- Proof:
{-
We distinguish the cases where t = Leaf and t = Node l v r .

mapTree id Leaf 
= -- e
Leaf

Assume the induction hypotheses that mapTree id l = l and mapTree id r = r.
Then,

mapTree id (Node l v r)
= -- f
Node (mapTree id l) (id v) (mapTree id r)
= -- d
Node (mapTree id l) v (mapTree id r)
= --I.H. 1
Node l v (mapTree id r)
= --I.H. 2
Node l v r

The claim now follows by induction.
-}


-- Question 4a

-- take 2 [1,2,3,4,5]
-- -->
-- 1 : take (2-1) [2,3,4,5]

-- map (+1) [1, undefined, 3]
-- -->
-- (1 + 1) : map (+) [undefined, 3]

-- foldr (\b acc -> b || acc) False [False, undefined, True]
-- -->
-- undefined

-- foldl (\acc b -> b || acc) False [False, undefined, True]
-- -->
-- True

-- seq (5, head []) 42
-- -->
-- 42


-- Question 4b
strictOr :: Bool -> Bool -> Bool
strictOr b1 b2 = seq b2 (b1 || b2)

-- Question 5a
ask :: String -> IO Bool
ask s = do
    putStrLn s
    k <- getChar
    return (k == 'y')

-- Question 5b
countYs :: [String] -> IO Int
countYs lst = do
    responses <- mapM ask lst
    return $ length $ filter id responses
-- or countYs = fmap (length . filter id) . mapM ask 

-- Question 5c
weird lst = let (anns, vals) = splitAt 5 lst in anns >>= \ann -> vals >>= \_ -> return ('c' : ann, True)

-- Question 5d
weird :: [String] -> [(String, Bool)]

-- Question 5e
-- []