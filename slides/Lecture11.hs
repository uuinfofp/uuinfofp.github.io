module Lecture11 where












foldr op e [x]
= -- desugar singleton
foldr op e (x : [])
= -- foldr, def 2
op x (foldr op e [])
= -- foldr, def 1 
op x e
= -- by assumption, e is neutral for op
x

-- where we remember that 
foldr op z [] = z 
foldr op z (x:xs) = op x (foldr op z xs)
op e x = x = op x e












foldl op e [x]
= -- desugar 
foldl op e (x:[])
= -- foldl def 1
foldl op (op e x) []
= -- assumption
foldl op x [] 
= -- foldl def 1
x

-- where we remember that
foldl op z [] = z
foldl op z (x : xs) = foldl op (op z x) xs
op e x = x = op x e














-- we want to prove that 
(f . (g . h))  -- two column style proof
=
((f . g) . h) 

-- by extensional reasoning, it enough to show that 
(f . (g . h)) x  -- two column style proof
=
((f . g) . h) x


(f . (g . h)) x  -- two column style proof
= -- def .
f ((g.h) x)
= -- def .
f (g (h x))
= -- def .
(f.g) (h x)
= -- def .
((f . g) . h) x


-- where we remember that 
(f . g) x = f (g x)


















map f :: [a] -> [b]
(x :) :: [a] -> [a]

-- we use extensional reasoning

(map f . (x :)) xs -- two column style proof, type? how do we start?
= -- def (.)
map f ((x:) xs)
= -- desugar section notation 
map f (x : xs)
= -- def map, 2 
f x : map f xs 
= -- desugar section notation
(f x :) (map f xs)
= -- def (.)
((f x :) . map f) xs

-- so (map f . (x :)) = ((f x :) . map f)
-- where 
a) map f [] = [] 
b) map f (x:xs) = f x : map f xs
c) (f. g) x = f (g x)
















-- not.not = id
not . not, id :: Bool -> Bool

-- to prove that not. not = id, it is enough to show that 
-- (not.not) b = id b for all Booleans b 

(not . not) b
= -- def .
not (not b)
= -- lemma a
b
= -- def id
id b


-- Lemma a: for all booleans b     not (not b) = b
not (not True)
=  -- not, 1
not False 
= -- not, 2
True 

not (not False)
= -- by def not, 2
not True 
= -- by def not, 1
False 

-- where
not True = False 
not False = True
(f. g) x = f (g x)
id x  = x





















data Nat = Zero | Succ Nat 

data Tree a = Leaf | Node (Tree a) a (Tree a)


data Expr a = Const Int 
            | Var String 
            | Plus (Expr a) (Expr a)










-- Claim:
map f (xs ++ ys)
= -- ...
map f xs ++ map f ys

-- We prove our claim through induction on xs, distinguishing the cases
-- xs = [] and xs = z : zs

-- Base case xs = [] 
map f ([] ++ ys)
= -- (++), def 1 
map f ys 
= -- (++), def q
[] ++ map f ys
= -- map, def 1
map f [] ++ map f ys

-- Inductive case xs = z :zs 
-- We assume the induction hypothesis (I.H.)
map f (zs ++ ys) = map f zs ++ map f ys 

map f ((z:zs) ++ ys)
= -- (++), def 2
map f (z : (zs ++ ys))
= -- map, def 2
f z : map f (zs ++ ys)
= -- I.H.
f z : (map f zs ++ map f ys)
= -- (++), def 2
(f z : map f zs) ++ map f ys
= -- map, def 2 
map f (z:zs) ++ map f ys

-- So, by induction on xs, our claim follows.

-- where
[] ++ ys  = ys
(x:xs) ++ ys = x : (xs ++ ys)

map f [] = [] 
map f (x:xs) = f x : map f xs





















map (f.g) -- :: [A] -> [B]

-- We use extensional reasoning and prove that 
map (f.g) xs 
= 
(map f . map g)  xs 
-- for all lists xs :: [A]

-- Claim:
map (f.g) xs
= -- lemma
map f (map g xs)
= -- def (.)
(map f . map g) xs

Lemma: map (f.g) xs = map f (map g xs) for all lists xs 

-- We prove this by induction on xs, distinguishing 
-- the cases xs = [] and xs = z:zs

-- Base case xs =[] 
map (f.g) [] 
= -- map, 1
[]
= -- map, 1
map f [] 
= -- map, 1
map f (map g [])

-- Inductive case xs = z:zs 
-- assuming the induction hypothesis 
map (f.g) zs = map f (map g zs) 


map (f.g) (z:zs)
= -- map, 2
(f.g)(z) : map (f.g) zs
= -- def (.)
f(g z) : map (f.g) zs
= -- I.H.
f (g z) : map f (map g zs)
= -- map, 2
map f (g z : map g zs)
= -- map, 2
map f (map g (z:zs))

-- So, our lemma follows by induction on xs



map f [] = [] 
map f (x:xs) = f x : map f xs 
(f.g) x = f (g x)





















(reverse.reverse) -- perhaps skip?
= -- ... 
id

-- where
id xs = xs 
reverse [] = []
reverse (x:xs) = reverse xs ++ [x] 
(f.g) xs = f (g xs)
[] ++ zs = zs 
(y:ys) ++ zs = y : (ys++zs)























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
-- We prove this by induction on t:
-- distinguish the cases that t= Leaf and t = Node l x r

-- Base case
size (mirror Leaf)
= -- mirror 1 
size Leaf


-- Inductive case
-- Assume the induction hypothesis 
size (mirror l) = size l 
AND 
size (mirror r) = size r

size (mirror (Node l v r))
= -- mirror, 2
size (Node (mirror r) v (mirror l))
= -- size, 2
1 + size (mirror r) + size (mirror l)
= -- commutative +
1 + size (mirror l) + size (mirror r)
= -- I.H.
1 + size l + size r
= -- size, 2
size (Node l v r)

-- The claim now follows by induction on t


size (mirror t)
= -- ...
size t

