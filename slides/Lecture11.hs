module Lecture11 where
    -- from last time:
    -- why is
    fNothing m = do 
        _ <- Nothing 
        x <- m
        return (x + 1)
    -- equal to
    Nothing 
    -- ???

    --
    do
        _ <- Nothing 
        x <- m
        return (x + 1)
    = -- desugaring do notation
    Nothing >>= \_ ->
    m >>= \x -> return (x + 1)
    = -- desugaring infix operator
    (>>=) Nothing (\_ ->(>>=) m (\x -> return (x+1)))
    = -- definition >>= for Maybe
    = Nothing

    -- where we remember that for Maybe, we have 
    -- (>>=) Nothing _  = Nothing
    -- (>>=) (Just x) f = Just (f x)

    foldr op e [x]
    = -- rewrite list syntactic sugar 
    foldr op e (x : [])
    = -- definition foldr
    op x (foldr op e [])
    = -- definition foldr
    op x e
    = -- by assump. e is right unit of op  
    x
    -- where we remember that 
    -- foldr op e [] = e 
    -- foldr op e (x:xs) = op x (foldr e xs)

    foldl op e [x]
    = -- rewrite list syntactic sugar 
    foldl op e (x:[])
    = -- definition foldl
    foldl op (op e x) []
    = -- definition foldl
    op e x
    = -- by assump. e is left unit of op 
    x

    -- where we remember that
    -- foldl op e [] = e 
    -- foldl op e (x : xs) = foldl op (op e x) xs

    (f . (g . h)) x  -- two column style proof
    = -- definition (.) 
    f ((g . h) x)
    = -- definition (.) 
    f (g (h x))
    = -- definition (.)
    (f . g) (h x)
    = -- definition (.)
    ((f . g) . h) x

    map f :: [a] -> [b]
    (x :) :: [a] -> [a]
    (map f . (x :)) xs -- two column style proof
    = -- def (.)
    map f ((x :) xs)
    = -- sugar sections 
    map f (x : xs)
    = -- def map 
    f x : map f xs 
    = -- sugar sections 
    (f x :) (map f xs)
    = -- def (.)
    ((f x :) . map f) xs

    -- not.not = id, first generic, then case distinction
    (not . not) x
    = -- def (.)
    not (not x)
    = -- case distinction of x 
    case x of False -> not (not False)
            | True -> not (not True)
    = -- def not 
    case x of False -> not True 
            | True -> not False
    = -- def not 
    case x of False -> False
            | True -> True
    = -- collapsing case distinction 
    x
    = -- def id 
    id x

    -- harder:
    do 
        x <- m
        _ <- Nothing 
        return (x + 1)
    = -- do syntactic sugar
    m >>= \x -> 
    (Nothing >>= \_ -> 
    return (x + 1))
    = -- definition of >>= for Maybe
    m >>= \x -> 
    Nothing
    = -- expand case
    case m of Nothing -> Nothing >>= \x -> Nothing 
            | Just y -> Just y >>= \x -> Nothing 
    = -- definition >>=
    case m of Nothing -> Nothing
            | Just y -> (\x -> Nothing) y
    = -- evaluating lambda expression
    case m of Nothing -> Nothing 
            | Just y -> Nothing
    = -- collapsing case
    Nothing 


    data Nat = Zero | Succ Nat 

    data Tree a = Leaf | Node (Tree a) a (Tree a)

    
    map f (xs ++ ys)
    = -- expand cases
    case xs of [] -> map f ([] ++ ys)
            | x : xs' -> map f ((x : xs') ++ ys)
    = -- definition ++
    case xs of [] -> map f ys
        | x : xs' -> map f ((x : xs') ++ ys)
    = -- definition ++
    case xs of [] -> [] ++ map f ys
    | (x : xs') -> f x : (map f (xs' ++ ys))
    = -- induction hypothesis: map f xs' ++ map f ys = map f (xs' ++ ys)
    case xs of [] -> [] ++ map f ys
    | (x : xs') -> f x : (map f xs' ++ map f ys)
    = -- definition ++ 
    case xs of [] -> [] ++ map f ys
    | (x : xs') -> f x : (map f xs') ++ map f ys
    = -- definition map f
    case xs of [] -> [] ++ map f ys
    | (x : xs') -> map f (x:xs') ++ map f ys
    = -- definition map f
    case xs of [] -> map f [] ++ map f ys
            | (x : xs') -> map f (x:xs') ++ map f ys
    = -- collapse cases
    map f xs ++ map f ys

    map (f.g) xs
    = -- expand cases
    case xs of [] -> map (f.g) []
            | (x : xs') -> map (f.g) (x : xs')
    = -- definition map, 2x
    case xs of [] -> []
            | (x : xs') -> (f.g) x : map (f.g) xs'
    = -- definition (.)
    case xs of [] -> []
    | (x : xs') -> f (g x) : map (f.g) xs'
    = -- induction hypothesis: map (f.g) xs' = (map f . map g) xs'
    case xs of [] -> []
    | (x : xs') -> f (g x) : (map f. map g) xs'
    = -- definition (.)
    case xs of [] -> (map f . map g) []
        | (x:xs') -> (f (g x): (map f (map g xs')))
    = --defintion map
    case xs of [] -> (map f . map g) []
        | (x:xs') -> map f  (g x: (map g xs'))
    = -- definition map
    case xs of [] -> (map f . map g) []
        | (x:xs') -> map f  (map g (x:xs'))
    = -- definition (.)
    case xs of [] -> (map f . map g) []
        | (x:xs') -> (map f . map g) (x:xs')
    = -- collapse cases
    (map f . map g) xs

    (reverse.reverse) []
    = -- def (.)
    reverse (reverse [])
    = -- def reverse
    reverse [] 
    = -- def reverse 
    [] 
    = -- def id 
    id [] 

    (reverse.reverse) (x:xs)
    = -- def (.)
    reverse (reverse (x:xs))
    = -- def reverse 
    reverse (reverse xs ++ [x])
    = -- lemma 1
    reverse [x] ++ reverse (reverse xs)
    = -- lemma 2
    [x] ++ reverse (reverse xs)
    =  -- def (.)
    [x] ++ (reverse.reverse) xs 
    = -- induction hypothesis: (reverse.reverse) xs
    [x] ++ xs 
    = -- def ++
    x : xs 
    = -- def id
    id (x : xs)

    -- lemma 2 : reverse [x] = [x]
    reverse [x]
    = -- sugar lists
    reverse (x : [])
    = -- def reverse
    reverse [] ++ [x] 
    = -- def reverse
    [] ++ [x]
    = -- def ++
    [x]

    -- lemma 1 : reverse (xs ++ ys) = reverse ys ++ reverse xs
    reverse ([] ++ ys)
    =
    reverse ys 
    = -- lemma 3
    reverse ys ++ [] 
    =
    reverse ys  ++ reverse [] 

    reverse ((x:xs) ++ ys)
    = 
    reverse (x (xs ++ ys))
    =
    reverse (xs ++ ys) ++ [x]
    = -- induction hypothesis: reverse (xs ++ ys) = reverse ys ++ reverse xs
    reverse ys  ++ reverse xs  ++ [x]
    =
    reverse ys ++ reverse (x:xs)

    -- lemma 3: ys ++ [] == ys
    [] ++ []
    = - def ++
    []

    (y:ys) ++ []
    =
    y : (ys ++ [])
    = -- induction hypothesis: ys ++ [] = ys
    y : ys 
    
    
    size Leaf = 0 
    size (Node l _ r) = 1+ size l + size r

    mirror Leaf = Leaf
    mirror (Node l v r) = Node (mirror r) v (mirror l)


    -- claim size (mirror t) = size t 
    size (mirror Leaf)
    =  -- def mirror
    size Leaf

    size (mirror (Node l v r))
    = -- def mirror
    size (Node (mirror r) v (mirror l))
    = -- def size
    1 + size (mirror r) + size (mirror l)
    = -- induction hypothesis: size(mirror r) = size r and size (mirror l) = size l
    1 + size r + size l 
    = -- + commutative
    1 + size l + size r
    = -- def size
    size (Node l v r)


    mult Zero Zero
    = 
    Zero

    mult (Succ n) Zero
    = 
    add Zero (mult n Zero)
    = -- induction hypothesis: mult n Zero  = Zero
    add Zero Zero 
    =
    Zero

    foldr op e []
    =
    e
    =
    foldl (flip op) e []

    foldr op e (x:xs)
    = -- def foldr
    x `op` (foldr op e xs)
    = -- induction hypothesis: foldr op e xs = foldl op e xs
    x `op` (foldl op e xs)
    = -- lemma 5
    foldl op (x `op` e) xs
    = -- assumption: e neutral for op
    foldl op x xs 
    = -- assumption: e neutral for op
    foldl op (e `op` x) xs  
    = -- def foldl
    foldl op e (x : xs)
    
    -- lemma 5: x `op` (foldl op acc xs) = foldl op (x `op` acc) xs
    x `op` (foldl op acc [])
    = -- def foldl 
    x `op` acc
    = -- def foldl
    foldl op (x `op` acc) []

    x `op` (foldl op acc (x':xs))
    = -- def foldl
    x `op` (foldl op (acc `op` x') xs) =
    = -- induction hypothesis: x `op` (foldl op acc' xs) = foldl op (x `op acc') xs
    foldl op (x `op` (acc `op` x')) xs
    = -- assumption: associativity of op
    foldl op ((x `op` acc) `op` x') xs
    = -- def foldl
    foldl op (x `op` acc) (x' : xs)
    
