module Lecture5 where

-- Recall -- derivative
differentiate :: (Double -> Double) -> Double -> Double
differentiate f a = (f (a + delta) - f a) / delta
  where
    delta = 0.00000001

-- Pattern matching on functions? NO!
-- differentiate' :: (Double -> Double) -> Double -> Double
-- differentiate' (\x -> x * x) = \x -> 2 * x
-- differentiate' sin = cos
-- differentiate' cos = negate . sin
-- differentiate' (f . g) = \x -> (differentiate' f) (g x) * differentiate' g x
-- uncurry
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

-- perimeter
data Point =
  Pt Float Float

data Shape
  = Rectangle Point Float Float
  | Circle Point Float
  | Triangle Point Point Point

perimeter :: Shape -> Float
perimeter (Rectangle _ w h) = 2 * w + 2 * h
perimeter (Circle _ r) = 2 * pi * r
perimeter (Triangle p1 p2 p3) = dist p1 p2 + dist p2 p3 + dist p3 p1
  where
    dist (Pt x1 y1) (Pt x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-- elemTree
data Tree a
  = EmptyTree
  | Node a (Tree a) (Tree a)

type IntTree = Tree Int

exampleTree =
  Node
    1
    (Node 2 EmptyTree EmptyTree)
    (Node
       3
       EmptyTree
       (Node 4 (Node 5 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)))

elemTree :: Int -> IntTree -> Bool
elemTree _ EmptyTree = False
elemTree i (Node j l r)
  | i == j = True
  | otherwise = elemTree i l || elemTree i r

-- treeHeight
treeHeight :: IntTree -> Int
treeHeight EmptyTree    = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- treeSize
treeSize :: IntTree -> Int
treeSize EmptyTree    = 0
treeSize (Node _ l r) = 1 + (+) (treeSize l) (treeSize r)

-- almostTreeFold
almostTreeFold :: (Int -> Int -> Int) -> IntTree -> Int
almostTreeFold _ EmptyTree       = 0
almostTreeFold op (Node _ lt rt) = 1 + op (treeHeight lt) (treeHeight rt)

-- almostTreeFold max  is treeHeight
-- almostTreeFold (+)  is treeSize
-- treeFoldr
treeFoldr :: (Int -> a -> a -> a) -> a -> IntTree -> a
treeFoldr _ init EmptyTree = init
treeFoldr op init (Node i l r) =
  op i (treeFoldr op init l) (treeFoldr op init r)

-- treeToList
data IntList
  = EmptyList
  | Cons Int IntList

concatList :: IntList -> IntList -> IntList
concatList EmptyList l2   = l2
concatList (Cons x xs) l2 = Cons x (concatList xs l2)

treeToList :: IntTree -> IntList
treeToList EmptyTree = EmptyList
treeToList (Node i lt rt) = Cons i (concatList (treeToList lt) (treeToList rt))

treeToList' = treeFoldr (\i lt rt -> Cons i (concatList lt rt)) EmptyList

-- find
find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x:xs)
  | p x = Just x
  | otherwise = find p xs

-- instance of Eq for Tree
instance Eq a => Eq (Tree a) where
  EmptyTree == EmptyTree = True
  (Node a l r) == (Node a' l' r') = a == a' && l == l' && r == r'
  _ == _ = False

-- some class example
class Shiftable a where
  hShift :: Float -> a -> a
  vShift :: Float -> a -> a
  dShift :: Float -> a -> a
  dShift s a = hShift s' (vShift s' a)
    where
      s' = s / sqrt 2

instance Shiftable Point where
  hShift s (Pt x y) = Pt (x + s) y
  vShift s (Pt x y) = Pt x (y + s)

instance Shiftable Shape where
  hShift s (Rectangle p w h) = Rectangle (hShift s p) w h
  hShift s (Circle p r) = Circle (hShift s p) r
  hShift s (Triangle p1 p2 p3) =
    Triangle (hShift s p1) (hShift s p2) (hShift s p3)
  vShift s (Rectangle p w h) = Rectangle (vShift s p) w h
  vShift s (Circle p r) = Circle (vShift s p) r
  vShift s (Triangle p1 p2 p3) =
    Triangle (vShift s p1) (vShift s p2) (vShift s p3)
