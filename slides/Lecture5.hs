module Lecture5 where

-- tuples -- AND
quad :: Float -> Float -> Float -> (Float, Float)
quad a b c = ( (-b + sqrt (b*b - 4*a*c)) / (2*a)
              , (-b - sqrt (b*b - 4*a*c)) / (2*a) )

-- Do not resort to writing solutions to mutable variables.



-- functions -- IMPLIES
show' :: Int -> String 
show' = show 














-- uncurry
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x,y) = f x y




curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)







-- perimeter
data Point = Pt Float Float

data Shape = Rectangle Point Float Float
           | Circle Point Float
           | Triangle Point Point Point

perimeter :: Shape -> Float
perimeter (Rectangle _ x y) = 2 * (x + y)
perimeter (Circle _ r) = 2 * pi * r ^ 2 
perimeter (Triangle p1 p2 p3) = distance p1 p2 + distance p2 p3 + distance p3 p1 where 
    distance (Pt x y) (Pt x' y') = sqrt ((x-x')^2 + (y-y')^2)





-- elemTree
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
type IntTree = Tree Int
exampleTree = Node
    1
    (Node 2 EmptyTree EmptyTree)
    (Node 3
          EmptyTree
          (Node 4 (Node 5 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree))
    )







elemTree :: Int -> IntTree -> Bool
elemTree _ EmptyTree = False 
elemTree x (Node y lt rt) = x == y || elemTree x lt || elemTree x rt







-- treeHeight
treeHeight :: IntTree -> Int
treeHeight EmptyTree = 0
treeHeight (Node _ lt rt) = 1 + max (treeHeight lt) (treeHeight rt)









-- treeSize
treeSize :: IntTree -> Int
treeSize EmptyTree = 0
treeSize (Node _ lt rt) = 1 + treeSize lt + treeSize rt











-- almostTreeFold
almostTreeFold :: (Int -> Int -> Int) -> IntTree -> Int
almostTreeFold op EmptyTree = 0
almostTreeFold op (Node _ lt rt) = 1 + op (almostTreeFold op lt) (almostTreeFold op rt)

-- almostTreeFold max  is treeHeight
-- almostTreeFold (+)  is treeSize










-- treeFoldr
treeFoldr :: (Int -> b -> b -> b) -> b -> IntTree -> b
treeFoldr op init EmptyTree = init 
treeFoldr op init (Node v lt rt) = op v (treeFoldr op init lt) (treeFoldr op init rt)

-- treeFoldr (\_ -> max) 0  is treeHeight
-- treeFoldr (\_ -> (+)) 0  is treeSize









-- treeToList
data IntList = EmptyList
             | Cons Int IntList

concatList :: IntList -> IntList -> IntList
concatList EmptyList   l2 = l2
concatList (Cons x xs) l2 = Cons x (concatList xs l2)

treeToList :: IntTree -> IntList
treeToList EmptyTree = EmptyList 
treeToList (Node v lt rt) = Cons v (concatList (treeToList lt) (treeToList rt))


-- using treeFoldr
treeToList' = treeFoldr op init where 
    init :: IntList
    init = EmptyList
    op :: Int -> IntList -> IntList -> IntList
    op x ll rl = Cons x (concatList ll rl)








-- find
find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing 
find p (x : xs) | p x = Just x 
                | otherwise = find p xs






data FunnyPair = FP Int Int
instance Eq FunnyPair where 
    FP x y == FP u v = x * v == y * u












-- instance of Eq for Tree
instance Eq a => Eq (Tree a) where
    EmptyTree == EmptyTree = True 
    (Node a lt rt) == (Node a' lt' rt') = a == a' && lt == lt'  && rt == rt'
    _ == _ = False












class EEq a where 
    eq :: a -> a -> Bool
    neq :: a -> a -> Bool 
    eq a a' = not (neq a a')
    neq a a' = not (eq a a')















-- some class example
class Shiftable a where
    hShift :: Float -> a -> a
    vShift :: Float -> a -> a
    dShift :: Float -> a -> a
    dShift s = hShift (s/sqrt 2) . vShift (s/sqrt 2)

instance Shiftable Point where
    hShift s (Pt x y) = Pt (x+s) y
    vShift s (Pt x y ) = Pt x (y+s)







instance Shiftable Shape where
    hShift s (Rectangle p w h) = Rectangle (hShift s p) w h
    hShift s (Circle p r     ) = Circle (hShift s p) r
    hShift s (Triangle p1 p2 p3) =
        Triangle (hShift s p1) (hShift s p2) (hShift s p3)
    vShift s (Rectangle p w h) = Rectangle (vShift s p) w h
    vShift s (Circle p r     ) = Circle (vShift s p) r
    vShift s (Triangle p1 p2 p3) =
        Triangle (vShift s p1) (vShift s p2) (vShift s p3)
