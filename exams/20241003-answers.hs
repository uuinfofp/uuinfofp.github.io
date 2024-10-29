-- Solutions for INFOFP Midterm 2024-2025

-- Question 1a
pressKeysChar :: Char -> (Int, Int)
pressKeysChar c = case c of
    'a' -> (2, 1); 'b' -> (2, 2); 'c' -> (2, 3)
    'd' -> (3, 1); 'e' -> (3, 2); 'f' -> (3, 3)
    'g' -> (4, 1); 'h' -> (4, 2); 'i' -> (4, 3)
    'j' -> (5, 1); 'k' -> (5, 2); 'l' -> (5, 3)
    'm' -> (6, 1); 'n' -> (6, 2); 'o' -> (6, 3)
    'p' -> (7, 1); 'q' -> (7, 2); 'r' -> (7, 3); 's' -> (7, 4)
    't' -> (8, 1); 'u' -> (8, 2); 'v' -> (8, 3)
    'w' -> (9, 1); 'x' -> (9, 2); 'y' -> (9, 3); 'z' -> (9, 4)
    _   -> error "Invalid character"

pressKeys :: String -> [Int]
pressKeys [] = []
pressKeys (c:cs) = let (i, j) = pressKeysChar c in 
                      replicate j i ++ pressKeys cs

-- Question 1b
letterCombs :: Int -> [Char]
letterCombs n = case n of
    2 -> "abc"; 3 -> "def"; 4 -> "ghi"
    5 -> "jkl"; 6 -> "mno"; 7 -> "pqrs"
    8 -> "tuv"; 9 -> "wxyz"
    _ -> error "Invalid key"

allLetterCombs :: [Int] -> [[Char]]
allLetterCombs = cartesianProd . map letterCombs

-- Question 1c
typoLetterCombs :: [Int] -> [[Char]]
typoLetterCombs = concatMap subsets . allLetterCombs

-- Question 1d
cartesianProd :: [[a]] -> [[a]]
cartesianProd [] = [[]]
cartesianProd (as:ass) = [a:bs | a <- as, bs <- cartesianProd ass]

-- Question 1e
subsets :: [a] -> [[a]]
subsets = foldr (\x acc -> acc ++ map (x:) acc) [[]]


-- Question 2a
-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
-- map :: (c -> d) -> [c] -> [d]
-- (observe that we have chosen fresh type variables)

-- so 'scanl map :: b -> [a] -> [b]' where:

-- b -> a -> b       ~         (c -> d) -> [c] -> [d]

-- since we filled in map as the first argument to scanl. This means that

-- b ~    c -> d
-- a ~    [c]
-- b ~    [d]

-- and thus

-- c -> d ~ [d]

-- so this is a type error.


-- Question 2b
-- map scanl :: [c] -> [d]
-- where

-- c -> d              ~       (b -> a -> b) -> b -> [a] -> [b]

-- since scanl first arg of map, so

-- c ~       b -> a -> b
-- d ~       b -> [a] -> [b]


-- thus,
-- map scanl :: [b -> a -> b] -> [b -> [a] -> [b]]


-- Question 3a
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

myTree = Node (Node (Leaf 4) 2 (Leaf 5))
              1
              (Node (Leaf 6) 3 (Node (Leaf 8) 7 (Leaf 9)))

eulerEdges :: Tree a -> Int
eulerEdges (Leaf _) = 0
eulerEdges (Node l _ r) = 4 + eulerEdges l + eulerEdges r

-- Question 3b
data Chain a = Single a | Multiple [(a, a)] deriving (Show, Eq)

instance Semigroup (Chain a) where
    Single x <> Single y = Multiple [(x, y)]
    Single x <> Multiple ys = Multiple ((x, fst (head ys)) : ys)
    Multiple xs <> Single y = Multiple (xs ++ [(snd (last xs), y)])
    Multiple xs <> Multiple ys = Multiple (xs ++ (fst (head ys), snd (last xs)) : ys)

-- Question 3c
eulerTour :: Tree a -> Chain a
eulerTour (Leaf x) = Single x
eulerTour (Node l x r) = Single x <> eulerTour l <> Single x <> eulerTour r <> Single x

-- Question 3d
makeUnique :: Tree a -> Tree (a,Int)
makeUnique = help 0
  where
    help n (Leaf x)     = Leaf (x,n)
    help n (Node l x r) = let l' = help n l
                              r' = help (1 + label l') r
                              m  = 1 + label r'
                                 in Node l' (x,m) r'

    label (Leaf (_,n)) = n
    label (Node _ (x,n) _) = n

-- Question 3e
fromEulerTour :: Eq a => Chain a -> Tree a
fromEulerTour (Single p) = Leaf p
fromEulerTour (Multiple ((p,x):edges)) = let removeFirstAndLast ys = tail $ init ys
                                             (ls,zs) = break (\e' -> e' == (x,p)) edges 
                                             (_,y) : rs = removeFirstAndLast zs
                                             left = case ls of [] -> Leaf x
                                                               _ -> fromEulerTour (Multiple ls)
                                             right = case rs of [] -> Leaf y
                                                                _ -> fromEulerTour (Multiple rs)          
                                         in 
                                         Node left p right

-- Question 3f
breakFold :: (a -> Bool) -> [a] -> ([a], [a])
breakFold p = foldr (\x (acc, rest) -> if p x then ([], x : acc ++ rest) else (x : acc, rest)) ([], [])



-- Question 4
-- For the final multiple-choice questions, we need to determine whether each Haskell expression evaluates to the list [1,2,3,4,5]. Here are the answers with brief explanations for each:

-- filter (\x -> 5 > x) [1..21]

-- Result: [1, 2, 3, 4]
-- Explanation: This expression filters out all elements less than 5 from the list [1..21]. Therefore, it does not evaluate to [1,2,3,4,5].
-- Answer: Incorrect


-- [f | f <- [1..10], g <- [1..10], f <= 5]

-- Result: [1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5]
-- Explanation: This list comprehension repeats all numbers from 1 to 5 for each g in [1..10], producing many repetitions, not a single [1,2,3,4,5].
-- Answer: Incorrect


-- [d `div` 2 | d <- [1..10], (d + 1) `div` 2 == d `div` 2]

-- Result: [1, 2, 3, 4, 5]
-- Explanation: This expression divides each number by 2 and filters out numbers where (d + 1) div2 == ddiv 2. This condition effectively selects odd numbers, which then get divided by 2, resulting in the list [1,2,3,4,5].
-- Answer: Correct


-- map (2+) (filter (>= -1) [-5 .. 3])

-- Result: [1, 2, 3, 4, 5]
-- Explanation: This expression first filters numbers greater than or equal to -1 from [-5..3], resulting in [-1,0,1,2,3]. Adding 2 to each of these produces [1,2,3,4,5].
-- Answer: Correct


-- [c + 1 | c <- [1..10], c < 4]

-- Result: [2, 3, 4]
-- Explanation: The expression takes elements less than 4 from [1..10] and adds 1 to each, producing [2, 3, 4]. It does not match [1,2,3,4,5].
-- Answer: Incorrect


-- map (+1) [b `div` 2 | b <- [1..10], b `mod` 2 == 1]

-- Result: [1, 2, 3, 4, 5]
-- Explanation: This expression divides each odd number from [1..10] by 2, giving [0,1,1,2,2,3,3,4,4,5]. Adding 1 to each unique result produces [1,2,3,4,5].
-- Answer: Correct


-- [a | a <- [1..10], a < 5]

-- Result: [1, 2, 3, 4]
-- Explanation: The comprehension selects elements less than 5, resulting in [1,2,3,4]. It does not match [1,2,3,4,5].
-- Answer: Incorrect

