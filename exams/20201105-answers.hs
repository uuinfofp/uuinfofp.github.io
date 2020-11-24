module Exam201105 where
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import System.Random


--------------------------------------------------------------------------------
-- * 1. Dequeues
--------------------
data Deque a = Empty 
             | Single a
             | Multiple (Access a) (Deque (a, a)) (Access a) deriving (Show, Eq)

data Access a = One a | Two a a deriving (Show, Eq)

mySmallDeque :: Deque Int
mySmallDeque = Multiple (One 1) (Single (2,3)) (Two 4 5)

myDeque :: Deque Int
myDeque = Multiple (Two 1 2) (Multiple (One (3,4)) (Single ((5,6),(7,8))) (One (9,10))) (One 11)

-- (a)
toList                  :: Deque a -> [a]
toList Empty            = []
toList (Single x)       = [x]
toList (Multiple l m r) = toList' l ++ flatten (toList m) ++ toList' r
  where
    toList' (One x)   = [x]
    toList' (Two x y) = [x,y]
    flatten = concatMap (\(x,y) -> [x,y])


-- (b)
safeLast :: Deque a -> Maybe a 
safeLast Empty = Nothing 
safeLast (Single a) = Just a 
safeLast (Multiple _ _ (One a)) = Just a 
safeLast (Multiple _ _ (Two _ a)) = Just a


-- (c)
cons :: a -> Deque a -> Deque a
cons a Empty = Single a 
cons a (Single b) = Multiple (One a) Empty (One b)
cons a (Multiple (One b) d e) = Multiple (Two a b) d e 
cons a q@(Multiple (Two b c) d e) = Multiple (One a) (cons (b,c) d) e 


-- (d) -- you may assume Access is already an instance of Functor
instance Functor Access where 
    fmap f (One a) = One (f a)
    fmap f (Two a a') = Two (f a) (f a')

instance Functor Deque where 
    fmap f Empty = Empty 
    fmap f (Single a) = Single (f a)
    fmap f (Multiple a d a') = Multiple (fmap f a) (fmap (\(x,y) -> (f x, f y)) d) (fmap f a')



--------------------------------------------------------------------------------
-- * 2. Equational reasoning
--------------------

-- (a) 

-- We use extensional reasoning. We need to show that flip bind const x y = id x y for all well-typed x and y.

-- flip bind const x y
-- = i 
-- bind x const y
-- = h 
-- const (x y) y 
-- = g 
-- x y
-- = e 
-- id x y



-- (b)

-- We use extensional reasoning. We need to show that foldl2 op e xs = foldl op e xs for all well-typed op e and xs.
-- We perform a case distinction on xs, distinguishing the cases xs = [] and xs = z:zs.
-- We prove the proposition by induction on xs.
-- We use the induction hypothesis that foldl op e zs = foldl2 op e zs for all op e.

-- (Base case)
-- foldl2 op e [] 
-- = f 
-- foldr (help op) id [] e
-- = c 
-- id e 
-- = e 
-- e
-- = a 
-- foldl op e [] 



-- (Inductive case)
-- foldl2 op e (z:zs)
-- = f 
-- foldr (help op) id (z:zs) e 
-- = d 
-- help op z (foldr (help op) id zs) e 
-- = j 
-- foldr (help op) id zs (op e z)
-- = f 
-- foldl2 op (op e z) zs
-- = I.H.
-- foldl op (op e z) zs
-- = b 
-- foldl op e (z:zs)

-- The claim now follows by induction.





--------------------------------------------------------------------------------
-- * 3. Graphs
--------------------


type Graph v = Map.Map v [v]

vertices :: Graph v -> [v]
vertices = Map.keys 


-- (a)
edges :: Graph v -> [(v,v)]
edges g = [(v,v') | (v,vs) <- Map.assocs g, v' <- vs]


-- (b)
neighbours :: Ord v => Graph v -> v -> [v]
neighbours g v = case Map.lookup v g of 
    Just vs -> vs 
    Nothing -> []


-- (c)
-- It is correct on acyclic graphs. On graphs with cycles it will produce an infinite list by recursing forever.
-- That is probably not the desired behaviour.


-- (d)

reachable :: Ord v => Graph v -> v -> [v]
reachable g v = snd $ runState (markVisited g v) []

markVisited :: Ord v => Graph v -> v -> State [v] ()
markVisited g v = do
                    visited <- get
                    if v `elem` visited then
                        do modify id
                    else do
                        put (v : visited)
                        mapM_ (markVisited g) $ neighbours g v



--------------------------------------------------------------------------------
-- * 4. IO and monads
--------------------
extract :: Int -> [a] -> ([a],a,[a]) 
extract i xs = (take i xs, xs !! i, drop (i+1) xs)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do i <- randomRIO (0,length xs - 1)
                let (pref,x,suf) = extract i xs
                xs' <- shuffle (pref ++ suf)
                return (x:xs')
                    
-- (a)

shuffle' :: [a] -> IO [a]
shuffle' [] = return []
shuffle' xs = randomRIO (0, length xs -1) >>= \i -> 
    let (pref, x, suf) = extract i xs in
        shuffle (pref ++ suf) >>= \xs' -> 
            return (x:xs')


-- (b)
foo :: IO Int
foo = do 
    putStrLn "input one or more Ints separated by spaces"
    line <- getLine
    let ss = words line
    ss' <- shuffle ss 
    putStrLn (unwords ss')
    let is = map read ss
    return (sum is)



--------------------------------------------------------------------------------
-- * 5. Testing
--------------------

-- (a)
noMoreSpaces :: (String -> [String]) -> String -> Bool
noMoreSpaces words' s = let ws = words' s in 
    all (\w -> notElem ' ' w) ws


-- (b)
removeInitialSpaces :: String -> String 
removeInitialSpaces = dropWhile (== ' ')

removeFinalSpaces :: String -> String 
removeFinalSpaces = reverse . removeInitialSpaces . reverse 


-- (c)
removeDuplicateSpaces :: String -> String
removeDuplicateSpaces [] = []
removeDuplicateSpaces (' ' : ' ' : as) = removeDuplicateSpaces (' ' : as)
removeDuplicateSpaces (a:as) = a : removeDuplicateSpaces as


-- (d)

-- No, this is not true. Some wordsImpl satisfying 'noMoreSpaces wordsImpl s' and 
-- 'recombines wordsImpl s' for all Strings 's' can include arbitrary empty strings 
-- in its output. We can fix the definition of recombines as follows:
recombines :: (String -> [String]) -> String -> Bool 
recombines wordsImpl s = let removeRedundantSpaces = removeFinalSpaces . removeInitialSpaces . removeDuplicateSpaces in  
    (unwords . wordsImpl) s == removeRedundantSpaces s


--------------------------------------------------------------------------------
-- * 6. Laziness
--------------------

-- (a)

-- ((:[]) 1) : map (:[]) [2]

-- Just ((3+2)+1)


-- (b)
force :: [a] -> [a]
force = foldr (\ x -> ($!) ((:) $! x)) []

-- OR SOMETHING LIKE
-- force        :: [a] -> [a]
-- force []     = []
-- force (x:xs) = let ys = force xs
--                in x `seq` (ys `seq` x:ys)


-- (c)
-- The easiest thing to do is to wrap (protect) and unwrap (unprotect) all elements in the lists in some constructor or lambda.
-- For example,
protect :: [a] -> [(Int, a)]
protect   = map (\x -> (0,x))

unprotect :: [(Int, a)] -> [a]
unprotect = map snd
