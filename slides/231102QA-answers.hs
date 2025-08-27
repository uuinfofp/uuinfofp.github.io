------------------------
-------- Proofs --------
------------------------

-- Using the definitions
data Tree a = Leaf | Node (Tree a) a (Tree a)
size :: Tree a -> Int
size Leaf = 0 -- a
size (Node l _ r) = (size l + 1) + size r -- b

mirror :: Tree a -> Tree a
mirror Leaf = Leaf -- c
mirror (Node l v r) = Node (mirror r) v (mirror l) -- d

choose :: Bool -> a -> a -> a 
choose True x y = x -- e
choose False x y = y -- f

flip :: (a -> b -> c) -> b -> a -> c 
flip f b a = f a b  -- g

not :: Bool -> Bool
not True = False -- h
not False = True -- i

(.) :: (b -> c) -> (a -> b) -> (a -> c) 
(.) f g a = f (g a) -- j

-- and laws
-- x + y = y + x -- k
-- (x + y) + z = x + (y + z) -- l

-- please prove the following two claims.
-- Please justify each reasoning step with the letter of a definition or law
-- or mark it with IH if you are using an induction hypothesis that you have assumed.
-- Please state any induction hypothesis you assume clearly.

-- a. Claim: size (mirror t) = size t 
{-
Proof:
We distinguish the cases t = Leaf (base case) and t = Node l v r (inductive case).

First, consider the base case of t = Leaf.

size (mirror Leaf)
= c
size Leaf 

Second, consider the inductive case of t = Node l v r.
Assume the induction hypotheses that
IH1: size l = size (mirror l)
IH2: size r = size (mirror r)

size (mirror (Node l v r))
= d
size (Node (mirror r) v (mirror l))
= b
(size (mirror r) + 1) + size (mirror l)
= IH1
(size (mirror r) + 1) + size l 
= IH2 
(size r + 1) + size l 
= -- k
(1 + size r) + size l 
= -- k 
size l + (1 + size r)
= --l
(size l + 1) + size r
=
size (Node l v r)

The claim now follows by induction on t.

-}


-- b. Claim: flip . choose = choose . not 
{- 
Proof:
By extensional reasoning, it is enough to prove that 
for all Booleans b and all a,a' of some type T, we have that 
(flip . choose) b a a' = (choose . not) b a a'.

We distinguish the cases of 
b = True and b = False 

First, consider the case of b = True

(flip . choose) True a a' 
=
flip (choose True) a a'
=
choose True a' a 
= 
a'
=
choose False a a'
=
choose (not True) a a' 
=
(choose . not) True a a'


Second, consider the case of b = False

(flip . choose) False a a' 
=
flip (choose False) a a'
=
choose False a' a 
= 
a
=
choose True a a'
=
choose (not False) a a' 
=
(choose . not) False a a'

Our claim follows.
-}


------------------------
------- Laziness -------
------------------------
-- a. Indicate, for each of the following expressions what their WHNF is. If the
-- expression is already in WHNF, please copy the original expression. If the
-- expression crashes in its evaluation to WHNF, please write "undefined".

-- 1. map undefined [1,2,3,4]
-- 2. foldr (+) 0 [1,2,3]
-- 3. filter even [1,2,3,4]
-- 4. Just (2 * 2)
-- 5. (\x -> 3 + 7) undefined
-- 6. (\x y -> x * y) 42
-- 7. foldl (+) 0 
-- 8. fst ((\x -> undefined) 25, [2 * 2])
-- 9. snd ((\x -> undefined) 25, [2 * 2])
-- 10. case [1,2,3] of 
--       []    -> True 
--       _ : _ -> False
-- 11. (\x -> 3 + 7)



-- 1. (undefined 1) : map undefined [2,3,4]
-- 2. 6
-- 3. 2 : filter even [3,4]
-- 4. Just (2 * 2)
-- 5. 10
-- 6. (\x y -> x * y) 42
-- 7. foldl (+) 0 
-- 8. undefined
-- 9. [2 * 2]
-- 10. False
-- 11. (\x -> 3 + 7)



-- b. Write a function 
force :: [a] -> [a]
-- that evaluates all the elements in the input list
-- to WHNF.

force [] = []
force (x:xs) = let ys = force xs in seq x (seq ys (x : ys))



------------------------
-------- Monads --------
------------------------
-- a. Recall that
-- 1. a Map k v is a data structure that associates keys of type k with values of type v, and allows us
-- to efficiently retrieve the value associated with a key, if it exists, using the function
lookup :: Ord k => k -> Map k v -> Maybe v
-- If the key does not occur in the Map, lookup returns a Nothing.
-- 2. Maybe is an instance of Monad.
-- Consider a function 
combineLookup :: Ord k => (v -> v -> Maybe b) -> k -> k -> Map k v -> Maybe b 
-- that looks up two keys in a Map k v (using the lookup function), and combines their
-- values using a user supplied function.
-- Here are some example uses of combineLookup, in which m = Map.fromList [(1,"foo"), (2,
-- "bar"), (6,""), (8,"baz")] is a Map that maps the key 1 to ”foo”, 2 to ”bar” etc.
-- > combineLookup (\v1 v2 -> Just (v1 ++ v2)) 1 2 m
-- Just "foobar"
-- > combineLookup (\v1 v2 -> if null v1 then Just v2 else Nothing) 1 2 m
-- Nothing
-- > combineLookup (\v1 v2 -> if null v1 then Just v2 else Nothing) 6 2 m
-- Just "bar"
-- > combineLookup (\v1 v2 -> if null v1 then Just v2 else Nothing) 3 2 m
-- Nothing

-- Using do-notation, please implement combineLookup.

combineLookup f k1 k2 m = do 
    v1 <- lookup k1 m 
    v2 <- lookup k2 m 
    f v1 v2



-- b. Translate the following piece of code using do-notation to using return and >>= directly.
main = do (fp:h:_) <- getArgs
          putStrLn h
          s <- readFile fp
          return (length s)

main =  getArgs >>= \(fp:h:_) ->
        putStrLn h >>= \_ ->
        readFile fp >>= \s ->
        return (length s)





-- c. Consider the following data type Log a, which annotates a value of type a with a list of
-- log messages, and the function withLogging that prints these messages to the terminal and then
-- returns the a:
data Log a = MkLog [String] a
withLogging :: Log a -> IO a
withLogging (MkLog l a) = do mapM_ putStrLn l
                             return a
-- Write a function 
log :: String -> Log () 
-- which logs a single message and returns a value of
-- type ().

log s = MkLog [s] ()



-- d. We can make Log an instance of Monad so that we can write nice logging code. For
-- example:
readInput :: Log Int
readInput = do log "about to read some input"
               return 5

computeSomething :: Log String
computeSomething = do i <- readInput
                      log "read some input"
                      let out = i * i
                      log "computed something"
                      return (show out)

computeIO :: IO String
computeIO = withLogging computeSomething

-- So that evaluating computeIO prints
-- about to read some input
-- read some input
-- computed something
-- to standard output and returns the string "25". Complete the Monad instance for Log, i.e. give the
-- implementation of
1. return :: a -> Log a
-- which does not log any messages
2. (>>=) :: Log a -> (a -> Log b) -> Log b
-- which collects all messages

instance Monad Log where 
    return a = MkLog [] a 
    (>>=) (MkLog l1 a) f = let (MkLog l2 b) = f a in MkLog (l1 ++ l2) b



-- e. Write a function withoutLogging :: Log a -> IO a that returns the a in Log a, but
-- does not actually print any log messages.

withoutLogging (MkLog _ a) = return a



-----------------------------------------------------
-------- Data types and Testing and Functors --------
-----------------------------------------------------

-- A heap is a data structure described by a data type quite similar to a search tree:
data Heap a = Top a (Heap a) (Heap a)
            | Empty deriving Show
-- with the so called (max) heap property that the a value in a Top node is larger than or equal to the
-- values in the roots of its child Heaps, which have this property themselves too. An example of a
-- heap is
aHeap = Top 10 (Top 7 Empty (Top 4 Empty Empty )) (Top 3 (Top 2 Empty Empty ) Empty )
-- Counter examples are
notAHeap1 = Top 6 (Top 7 Empty (Top 4 Empty Empty )) (Top 3 (Top 2 Empty Empty ) Empty )
notAHeap2 = Top 10 (Top 7 Empty (Top 4 Empty Empty )) (Top 1 (Top 2 Empty Empty ) Empty )

-- a. Give a Functor instance for Heap 

instance Functor Heap where 
    fmap f (Top v l r) = Top (f v) (fmap f l) (fmap f r)
    fmap f Empty = Empty




-- b. Write a function
checkHeap :: Ord a => Heap a -> Bool
-- which returns True if its argument has
-- the required propery, and False otherwise.
-- Hint: you may want to write a helper function
checkHeap' :: Ord a => Heap a -> (Maybe a, Bool)
checkHeap = snd . checkHeap'

checkHeap' Empty = (Nothing, True)
checkHeap' (Top v l r) = let (ml, hl) = checkHeap' l in let (mr, hr) = checkHeap' r in let mv = Just v in (mv, hl && hr && (mv >= ml) && (mv >= mr))


checkHeapWith :: Ord a => a -> Heap a -> Bool
checkHeapWith v Empty = True 
checkHeapWith v (Top v' l r) = v >= v' && checkHeapWith v' l && checkHeapWith v' r
checkHeap2 Empty = True
checkHeap2 h@(Top v l r) = checkHeapWith v h

-- c. Write a function
getFromHeap :: Ord a => Heap a -> Maybe (a, Heap a)
-- that – provided the heap is non-empty–
-- returns the largest a value from its Heap a argument, together with
-- a Heap a containing the rest of the values of its Heap a argument, and Nothing if the Heap a
-- argument is Empty.
-- Make sure the resulting Heap a again satisfies the heap property!
-- Hint: the new root of the heap is either the root of the left subtree or the root of the right subtree.
getFromHeap Empty = Nothing
getFromHeap (Top v l r) = Just (v, mergeHeaps l r)

mergeHeaps :: Ord a => Heap a -> Heap a -> Heap a 
mergeHeaps Empty b = b
mergeHeaps a Empty = a
mergeHeaps a@(Top v l r) b@(Top v' l' r') | v <= v' = Top v' (mergeHeaps a l') r' 
                                          | otherwise = Top v (mergeHeaps b l) r 




-- d. Formulate a specification stillAHeap to test that the heap property is preserved
-- when removing an element from a heap using getFromHeap.
stillAHeap :: Ord a => Heap a -> Bool
stillAHeap h = case getFromHeap h of 
    Nothing -> True 
    Just (_, h') -> checkHeap h'