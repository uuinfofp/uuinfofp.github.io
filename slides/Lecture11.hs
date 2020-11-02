module Lecture11 where












    foldr op e [x]
    = -- desugaring
    foldr op e (x : [])
    = -- def. foldr, 2
    op x (foldr op e [])
    = -- def. foldr, 1
    op x e
    = -- assumption that e is the unit element for op
    x

    -- where we remember that 
    foldr op e [] = e 
    foldr op e (x:xs) = op x (foldr op e xs)
    -- assumption: op e x = x = op x e












    foldl op e [x]
    = -- desugar
    foldl op e (x : [])
    = -- def. foldl, 2    
    foldl op (op e x) []
    = -- assumption: op e x = x = op x e   
    foldl op x []
    = -- def. foldl, 1
    x

    -- where we remember that
    foldl op e [] = e 
    foldl op e (x : xs) = foldl op (op e x) xs
















    (f . (g . h)) x  -- two column style proof
    = -- def (.), left to right
    f ((g. h) x)
    = -- def (.), left to right
    f (g (h x))
    = -- def (.), right to left
    (f . g) (h x)
    = -- def (.), right to left
    ((f . g) . h) x

    -- so we conclude that 
    f. (g. h)= (f.g) . h


    -- where we remember that 
    (f . g) x = f (g x)


















    map f :: [a] -> [b]
    (x :) :: [a] -> [a]

    (map f . (x :)) xs -- two column style proof
    = -- def (.)
    map f ((x :) xs)
    = -- section notation
    map f (x : xs)
    = -- def map, non-empty lists
    f x : map f xs
    = -- section notation
    (f x :) (map f xs)
    = -- def (.)
    ((f x :) . map f) xs

    -- so (map f . (x :)) = ((f x :) . map f)


















    -- not.not = id, first generic, then case distinction
    not . not, id :: Bool -> Bool

    -- Enough: show (not.not) b = id b for all b :: Bool.
    -- We perform a case distinction on b: b is either True or False

    (not . not) True 
    = -- def. (.)
    not (not True)
    = -- def not
    not False
    = -- def not
    True
    = -- def. id
    id True

    (not . not) False 
    = -- def. (.)
    not (not False)
    = -- def not
    not True
    = -- def not
    False
    = -- def. id
    id False

    -- So we conclude that not.not = id

    not True = False 
    not False = True
























    data Nat = Zero | Succ Nat 

    data Tree a = Leaf | Node (Tree a) a (Tree a)

    











    -- Claim:
    map f (xs ++ ys)
    = -- ...
    map f xs ++ map f ys

    -- Let us perform a case distinction on xs: xs = [] or xs = z : zs.
    -- Then we need to prove that 

    -- Base case in our induction:
    map f ([] ++ ys)
    = -- def (++), 1
    map f ys
    = -- def (++), 1
    [] ++ map f ys
    = -- def map, 1
    map f [] ++ map f ys


    -- Inductive case:
    -- Let us assume the Induction Hypothesis (I.H.) that map f (zs ++ ys) = map f zs ++ map f ys.
    map f ((z:zs) ++ ys)
    = -- def (++), 2
    map f (z : (zs ++ ys))
    = -- def map, 2
    f z : map f (zs ++ ys)
    = -- I.H.
    f z : (map f zs ++ map f ys)
    = -- def (++), 2
    (f z : map f zs) ++ map f ys
    = -- def map, 2
    map f (z:zs) ++ map f ys

    -- So, we can conclude that our claim is true for all finite lists, by induction on xs.




    [] ++ ys  = ys
    (x:xs) ++ ys = x : (xs ++ ys)

    map f [] = [] 
    map f (x:xs) = f x : map f xs





















    f :: T -> S 
    g :: R -> T 
    f .g :: R -> S 
    map (f.g) :: [R] -> [S]

    -- Claim:
    map (f.g)
    = -- ...
    (map f . map g)

    -- Take some xs :: [R]. Then, xs = [] or xs = z : zs.
    -- Then we need to show that 
    -- Base case:
    map (f.g) []
    = -- def map, []
    []
    = -- def map, []
    map f []
    = -- def map, []
    map f (map g [])
    = -- def (.)
    (map f.map g) []


    -- Inductive case, assuming the Induction Hypothesis that map (f.g) zs = (map f. map g) zs:
    map (f.g) (z:zs)
    = -- def map, rec
    (f.g) z : map (f.g) zs
    = -- def (.)
    f (g z) : map (f.g) zs
    = -- I.H.
    f (g z) : (map f.map g) zs
    = -- def (.)
    f (g z) : map f (map g zs)
    = -- def map, rec.
    map f (g z : map g zs)
    = -- def map, rec.
    map f (map g (z: zs))
    = -- def (.)
    (map f .map g) (z:zs)

    -- The claim now follows by induction on xs.























    (reverse.reverse)
    = -- ... 
    id


























    -- lemma 2 : reverse [x] = [x]
    reverse [x]
    = -- ...
    [x]














    -- Homework: lemma 1 : reverse (xs ++ ys) = reverse ys ++ reverse xs
    -- Hint: prove and use lemma 3 below!
    reverse (xs ++ ys)
    = -- ...
    reverse ys ++ reverse xs





















    -- lemma 3: ys ++ [] == ys
    ys ++ []
    = -- ...
    ys





















    
    size Leaf = 0 
    size (Node l _ r) = 1+ size l + size r

    mirror Leaf = Leaf
    mirror (Node l v r) = Node (mirror r) v (mirror l)





    -- claim: size (mirror t) = size t 
    -- We prove this by induction on t: distinguish the cases that t= Leaf and t = Node l x r

    -- Base case:

    size (mirror Leaf)
    = -- def mirror, 1
    size Leaf


    -- Inductive case, assume the induction hypothesis that size (mirror l) = size l AND size  (mirror r) = size r
    size (mirror (Node l x r))
    = -- def mirror, 2
    size (Node (mirror r) x (mirror l))
    = -- def size, 2
    1 + size (mirror r) + size (mirror l)
    = -- I.H.
    1 + size r + size l
    = -- (+) is commutative
    1 + size l + size r
    =  -- def size, 2
    size (Node l x r)









