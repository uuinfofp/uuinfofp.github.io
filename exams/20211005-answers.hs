-- Midterm 2021-10-05

-- TOPICS:
-- basic programming: 1a 3a 6b
-- recursion on lists: 1b 2a
-- recursion on trees: 6a 6d
-- data structures (preferably trees):  1a 1b 3a 6a 6b 6d
-- list comprehensions: 1c
-- higher order functions, basic: 4a 4b
-- higher order functions, hard: 2b
-- type classes: 3b
-- type inference: 5a 5b

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * 1.
--------------------------------------------------------------------------------

-- a)
safeHead :: [a] -> Maybe a

-- it can be implemented as
safeHead [] = Nothing
safeHead (x:_) = Just x


-- b)

-- | if found  returns the (key, value) and the rest of the list.
lookupExtract :: Eq k => k -> [(k,v)] -> (Maybe (k,v), [(k,v)])
-- together: give type here
lookupExtract _ []                      = (Nothing, [])
lookupExtract k (t@(x,_):m) | k == x    = (Just t, m)
                            | otherwise = let (r,m') = lookupExtract k m in (r, t:m')

-- Some people removed all occurances of key k from the
-- remainder of the list. I also counted that as correct.

-- c)

lookupKV     :: Eq k => k -> [(k,v)] -> Maybe (k,v)
lookupKV k m = safeHead [ (x,v) | (x,v) <- m, x == k]


--------------------------------------------------------------------------------
-- * 2.
--------------------------------------------------------------------------------


buckets   :: Int -> [a] -> [[a]]
buckets k = snd . buckets' k

-- a)

bucketsH          :: Int -> [a] -> (Int, [[a]])
bucketsH k []     = (0,[[]])
bucketsH k (x:xs) = case bucketsH k xs of
                      (l,b:bs) | l < k     -> (l+1, (x:b):bs)     -- add to current bucket
                               | otherwise -> (1,   [x] : b : bs) -- start a new bucket
                      _    -> error "empty list does not actually occur"

-- having the error case was not required to get full points.

-- b)

concatMap g = foldr f e
  where
    f x r = g x ++ r
    e     = []

--------------------------------------------------------------------------------
-- * 3.
--------------------------------------------------------------------------------

data Successor a
succOf :: Ord a => a -> Successor a -> Maybe a
succOf = undefined

data Key k v = Key k (Maybe v)
type Map k v = Successor (Key k v)

-- a)

lookupInMap     :: Ord k => k -> Map k v -> Maybe v
lookupInMap k m = case succOf (Key k Nothing) m of
                    Nothing                      -> Nothing
                    Just (Key k' mv) | k == k'   -> mv
                                     | otherwise -> Nothing

-- b)
instance Eq k => Eq (Key k v) where
  (Key a _) == (Key b _) = a == b

-- you were only asked for the Eq instance. For the implementation of
-- a to work you also need the def of Ord which looks very similar:

instance Ord k => Ord (Key k v) where
  (Key a _) `compare` (Key b _) = a `compare` b

--------------------------------------------------------------------------------
-- * 4.
--------------------------------------------------------------------------------

-- a)
mystery :: [a] -> [a]
mystery = foldl (flip (:)) []

-- It reverses the input list, i.e. mistery = reverse

-- b)
foo yzs = [y + z | (y, z) <- yzs, y > 2]

foo' :: (Num a, Ord a) => [(a, a)] -> [a]
foo' = map (uncurry (+)) . filter ((>2). fst)

-- you could also just write this using lambdas:
foo'' = map (\(y,z) -> y + z) . filter (\(y,_) -> y > 2)

-- Some people also implemented this using a fold, which was also fine:

foo''' = foldr f []
  where
    f (y,z) r | y > 2     = y + z : r
              | otherwise = r
  -- note that the result is a list, not a number.

--------------------------------------------------------------------------------
-- * 5.
--------------------------------------------------------------------------------


-- Determine the type of the expressions

-- a) foldl (flip (:))

{-

We have:

flip  :: (a -> b -> c) -> b -> a -> c
foldl :: (d -> e -> d) -> d -> [e] -> d
(:)   :: f -> [f] -> [f]

we fill in (:) as the first argument of flip, so their types should be equal:
a -> b -> c ~ f -> [f] -> [f]

hence
a ~ f
b ~ [f]
c ~ [f]

since we already filled in the first argument of flip, we have:

flip (:) :: b -> a -> c

which, given the constraints, is thus

flip (:) :: [f] -> f -> [f]


We fill in 'flip (:)' as the first argument to foldl, so we learn that

[f] -> f -> [f] ~ d -> e -> d

and thus

[f] ~ d
f ~ e
(and again that [f] ~ d)

since we already filled in the first argument to foldl we have

foldl (flip (:)) :: d -> [e] -> d

which, given the constraints, is thus

foldl (flip (:)) :: [f] -> [f] -> [f]

-}


-- b) concat . map concat

-- answer b:
-- [[[a]]] -> [a]


--------------------------------------------------------------------------------
-- * 6.
--------------------------------------------------------------------------------

data Trie = Leaf Bool
          | Node Bool [(Char,Trie)]
            deriving (Show,Eq)

-- a)

suffixes :: [Char] -> Trie -> Trie
suffixes [] t = t
suffixes (_:_) (Leaf _) = Leaf False -- not found
suffixes (c:cs) (Node _ chs) = case lookup c chs of
                                 Nothing -> Leaf False
                                 Just t  -> suffixes cs t

-- b)

occurs  :: [Char] -> Trie -> Bool
occurs s t = case suffixes s t of
               Leaf b   -> b
               Node _ _ -> True

-- We didn't specify well if 'occurs' means occurs exactly or occurs
-- as a prefix (the above implementation assumes a prefix is
-- fine). We've considered both to be correct. If you want to test for
-- an exact match the 'Node' case becomes 'Node b _ -> b' instead.

-- c,d,e)

insert                     :: [Char] -> Trie -> Trie
insert []     (Leaf _)     = Leaf True
insert []     (Node _ chs) = Node True chs
insert (c:cs) (Leaf b)     = Node b [(c,insert cs $ Leaf False)]
insert (c:cs) (Node b chs) = Node b $ case lookupExtract c chs of
                                        (Nothing, _)    -> (c,insert cs $ Leaf False) : chs
                                        (Just (_,t),ts) -> (c,insert cs t) : ts

-- that so the ansers are

-- c)
-- Leaf True
-- d)
-- Node True chs
-- e)
-- (c,insert cs t) : ts
