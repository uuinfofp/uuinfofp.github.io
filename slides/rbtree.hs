import Data.Maybe(isJust)
import Prelude hiding (lookup)


data Color = Red | Black deriving (Show,Eq)

data RBTree a = Leaf
              | Node Color (RBTree a) a (RBTree a)
              deriving (Show,Eq)

-- why not Leaf Color


-- properties:
-- 1) leaves are black
-- 2) red nodes have black children
-- 3) root is black
-- 4) for any node, all paths to leaves have the same number of black children.

--
-- Just h -> all paths have length h
-- Nothing -> not a valid RB tree
blackHeight                    :: RBTree a -> Maybe Int
blackHeight Leaf               = Just 1
blackHeight (Node Black l _ r) = blackHeight' 1 (blackHeight l) (blackHeight r)
blackHeight (Node Red   l _ r) = blackHeight' 0 (blackHeight l) (blackHeight r)

blackHeight' _ Nothing   _                     = Nothing
blackHeight' _ _         Nothing               = Nothing
blackHeight' z (Just lh) (Just rh) | lh == rh  = Just (z + lh)
                                   | otherwise = Nothing

blackChildren                    :: RBTree a -> Bool
blackChildren Leaf               = True
blackChildren (Node Black l _ r) = blackChildren l && blackChildren r
blackChildren (Node Red   l _ r) = rootColor l == Black
                                && rootColor r == Black
                                && blackChildren l && blackChildren r

rootColor Leaf           = Black
rootColor (Node c _ _ _) = c


validRBTree      :: RBTree a -> Bool
validRBTree root = and [ blackChildren root
                       , rootColor root == Black
                       , isJust (blackHeight root)
                       ]


-- * Search

lookup                              :: Ord a => a -> RBTree a -> Maybe a
lookup _ Leaf                       = Nothing
lookup x (Node _ l y r) | x < y     = lookup x l
                        | x == y    = Just y
                        | otherwise = lookup x r

-- * insert

insert   :: Ord a => a -> RBTree a -> RBTree a
insert x = blackenRoot . insert' x

blackenRoot                :: RBTree a -> RBTree a
blackenRoot Leaf           = Leaf
blackenRoot (Node _ l y r) = Node Black l y r


-- black heights are ok; since we replaced a black leaf by a red node
--
-- only trouble is possible red,red violations
-- allow red-red violations with the root (but not below that)
insert'                    :: Ord a => a -> RBTree a -> RBTree a
insert' x Leaf             = Node Red Leaf x Leaf
insert' x t@(Node c l y r) | x < y     = balance c (insert x l) y r
                           | x == y    = t
                           | otherwise = balance c l            y (insert x r)

-- idea of balance: only possible ting that can be wrong is two red nodes near the root
-- (and in only one of the four places).
-- so make the root red, and its children black

-- ok because: red may be violated
-- coloring a red node black

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a


balance Black (Node Red (Node Red a x b) y c) z d = Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d = Node Red (Node Black a x b) y (Node Black c z d)

balance Black a x (Node Red (Node Red b y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) = Node Red (Node Black a x b) y (Node Black c z d)

balance c l x r                                   = Node c l x r

-- * Deletion are more messy

-- but can be defined similarly


fromList :: Ord a => [a] -> RBTree a
fromList = foldr insert Leaf

toList Leaf = []
toList (Node c l y r) = toList l ++ [y] ++ toList r

-- assuming elements are unique (can be adapted)
sort :: Ord a => [a] -> [a]
sort = toList . fromList

-- * In haskell

-- Data.Set
-- Data.Map
