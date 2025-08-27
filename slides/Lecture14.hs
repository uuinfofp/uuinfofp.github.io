module Lecture14 where
import Data.Functor.Identity

-- THREE DATA TYPES --
data Tree a
   = Leaf
   | Node (Tree a) a (Tree a)
   deriving (Show)

exampleTree = Node (Node (Node Leaf (-6) Leaf) 64 Leaf) 55 Leaf

data Rose a
   = RNode a [Rose a]
   | RLeaf
   deriving (Show)

exampleRose =
   RNode (-6) [RNode 24 [], RLeaf, RNode 52 [RNode 53 [], RNode 35 []]]

exampleRose2 =
   RNode
   True
   [RNode False [], RLeaf, RNode False [RNode False [], RNode True []]]

data Expr x
   = Var x
   | Val Int
   | Add (Expr x) (Expr x)
   deriving (Show)

exampleExpr :: Expr String
exampleExpr =
   Add
   (Add (Add (Var "x") (Val 2)) (Add (Add (Var "y") (Var "z")) (Val 3)))
   (Var "w")



-- MONOIDS AND FOLDABLES --
-- What do the operations of calculating a product of elements of a tree, the parity of elements of a rosetree,
-- and the free variables in an expression have in common?
-- They are examples of a notion of fold!
-- For a list, we would use a fold. Can we do something similar here?

-- MONOIDS, EXAMPLES --
class Semigroup' m where
   (<>.) :: m -> m -> m

class Semigroup' m =>
       Monoid' m
   where
   mempty' :: m
   mempty' = mconcat' []
   mappend' :: m -> m -> m
   mappend' = (<>.)
   mconcat' :: [m] -> m
   mconcat' = foldr mappend' mempty'
   -- mconcat' []       = mempty'
   -- mconcat' (x : xs) = x <> mconcat' xs

instance Semigroup' [a] where
   (<>.) = (++)

instance Monoid' [a] where
   mempty' = []

newtype End b =
   End
   { getEnd :: b -> b
   }

instance Semigroup (End a) where
   (<>) (End f) (End g) = End (f . g)

instance Monoid (End a) where
   mempty = End id

newtype Sum a =
   Sum
   { getSum :: a
   }

instance Num a => Semigroup (Sum a) where
   (<>) (Sum x) (Sum y) = Sum (x + y)

instance Num a => Monoid (Sum a) where
   mempty = Sum 0

newtype Product a =
   Product
   { getProduct :: a
   }

instance Num a => Semigroup (Product a) where
   (<>) (Product x) (Product y) = Product (x * y)

instance Num a => Monoid (Product a) where
   mempty = Product 1

newtype Any =
   Any
   { getAny :: Bool
   }

newtype All =
   All
   { getAll :: Bool
   }

newtype XOR =
   XOR
   { getXOR :: Bool
   }

instance Semigroup Any where
   (<>) (Any x) (Any y) = Any (x || y)

instance Monoid Any where
   mempty = Any False
instance Semigroup All where
  (<>) (All x) (All y) = All (x && y)

instance Monoid All where
  mempty = All True

instance Semigroup XOR where
  (<>) (XOR True) (XOR True) = XOR False
  (<>) (XOR x) (XOR y)       = XOR (x || y)

instance Monoid XOR where
  mempty = XOR False


  -- Does foldMap buy us anything?

foldr__ :: (a -> b -> b) -> b -> [a] -> b
foldr__ op i ta = getEnd (foldMap (End . op) ta) i

foldMap__ :: Monoid m => (a -> m) -> [a] -> m
foldMap__ f = mconcat . map f
   



-- FOLDABLES --

class Functor t =>
      Foldable' t
  where
  toList' :: t a -> [a]
       --toList' = foldr' (:) []
  toList' = foldMap' return
  foldr' :: (a -> b -> b) -> b -> t a -> b
  foldr' op i = foldr op i . toList'
       -- foldr' op i ta = getEnd (foldMap' (End . op) ta) i
  foldMap' :: Monoid m => (a -> m) -> t a -> m
       -- foldMap' f = mconcat . map f . toList'
  foldMap' f = mconcat . map f . foldr' (:) []
  fold' :: Monoid m => t m -> m
  fold' = foldMap' id

instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Foldable Tree where
  foldMap _ Leaf         = mempty
  foldMap f (Node l v r) = (foldMap f l) <> (f v) <> (foldMap f r)

instance Functor Rose where
  fmap _ RLeaf        = RLeaf
  fmap f (RNode v rs) = RNode (f v) (map (fmap f) rs)

instance Foldable Rose where
  foldMap _ RLeaf        = mempty
  foldMap f (RNode v rs) = (f v) <> mconcat (map (foldMap f) rs)

instance Functor Expr where
  fmap f (Var x)     = Var (f x)
  fmap f (Add t1 t2) = Add (fmap f t1) (fmap f t2)
  fmap _ (Val i)     = Val i

instance Foldable Expr where
  foldMap f (Var x)     = f x
  foldMap f (Add t1 t2) = foldMap f t1 <> foldMap f t2
  foldMap _ (Val i)     = mempty

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

toList :: Foldable t => t a -> [a]
toList = foldr (:) []



   -- Our 3 examples

prod :: (Foldable t, Num a, Functor t) => t a -> a
prod = getProduct . fold . fmap Product

parity :: (Foldable t, Functor t) => t Bool -> Bool
parity = getXOR . fold . fmap XOR

usedVars :: (Foldable t, Functor t) => t String -> [String]
usedVars = toList

exampleProd = prod exampleTree

exampleParity = parity exampleRose2

exampleVars = usedVars exampleExpr





-- APPLICATIVES AND TRAVERSABLES --
-- APPLICATIVES --
data Error m a = Error m
              | OK a  deriving (Show)

instance Functor (Error m) where
   fmap f (OK    a) = OK (f a)
   fmap f (Error m) = Error m

instance Monoid m => Applicative (Error m) where
   pure = OK
   (Error m1) <*> (Error m2) = Error (m1 <> m2)
   (Error m1) <*> (OK    a ) = Error m1
   (OK    f ) <*> (Error m2) = Error m2
   (OK    f ) <*> (OK    a ) = OK (f a)


type ErrorS = Error [String]

-- TRAVERSABLES --
class Foldable' t =>
       Traversable' t
   where
   traverse' :: Applicative f => (a -> f b) -> t a -> f (t b)
   traverse' g ta = sequenceA' (fmap g ta)
   sequenceA' :: Applicative f => t (f a) -> f (t a)
   sequenceA' = traverse' id

instance Traversable Tree
   -- sequence :: Applicative f => t (f a) -> f (t a)
                                                       where
   sequenceA Leaf = pure Leaf
   sequenceA (Node l v r) =
       pure Node <*> (sequenceA l) <*> v <*> (sequenceA r)

instance Traversable Rose where
   sequenceA RLeaf = pure RLeaf
   sequenceA (RNode v rs) =
       pure RNode <*> v <*> (sequenceA (map sequenceA rs))

instance Traversable Expr where
   traverse f (Var x  ) = pure Var <*> f x
   traverse f (Val i  ) = pure (Val i)
   traverse f (Add s t) = pure Add <*> traverse f s <*> traverse f t




-- Our 3 examples --
traverseAndPrint :: (Show a, Traversable t) => t a -> IO (t ())
traverseAndPrint = traverse print

exampleTravPrint = traverseAndPrint exampleRose2

check :: (Num a, Show a, Ord a) => a -> ErrorS a
check x | x < -3 = Error ["We found a value " ++ show x ++ " which is below 3."]
check x | x > 4 = Error ["We found a value " ++ show x ++ " which is above 4."]
check x | otherwise = pure x

traverseAndCheck :: (Num a, Show a, Ord a, Traversable t) => t a -> ErrorS (t a)
traverseAndCheck = traverse check

exampleTravCheck = traverseAndCheck exampleTree

lookupError :: [(String, a)] -> String -> ErrorS a
lookupError m s = case lookup s m of
   Just x -> OK x
   Nothing ->
       Error ["Variable " ++ s ++ " does not have an associated value."]
ev :: [(String, Int)] -> Expr String -> ErrorS (Expr Int)
ev m = traverse (lookupError m)


exampleMap1 :: [(String, Int)]
exampleMap1 =
   [("x", 42), ("y", 666), ("z", 23), ("w", 20), ("w", 21), ("c", 243)]
exampleMap2 :: [(String, Int)]
exampleMap2 = [("x", 42), ("w", 20), ("w", 21), ("c", 243)]
exampleTravLookup1 = ev exampleMap1 exampleExpr
exampleTravLookup2 = ev exampleMap2 exampleExpr




-- An example from last week --

newtype State s a =
   S (s -> (a, s))

instance Functor (State a) where
   fmap f mb = do
       b <- mb
       return (f b)

instance Applicative (State a) where
   pure b = S (\a -> (b, a))
   mf <*> mb = do
       f <- mf
       b <- mb
       return (f b)

instance Monad (State a) where
    (S b) >>= f = S
        (\a ->
            let (b', a') = b a
            in  case f b' of
                    S f' -> f' a'
        )

run :: State s a -> s -> a
run (S f) s = let (a, _) = f s in a

label :: a -> State Int (a, Int)
label v = S (\i -> ((v, i), i + 1))

traverseAndLabel :: Traversable t => t a -> t (a, Int)
traverseAndLabel t = run (traverse label t) 0

exampleTravLabel1 = traverseAndLabel exampleTree
exampleTravLabel2 = traverseAndLabel exampleRose2
exampleTravLabel3 = traverseAndLabel exampleExpr




-- TRAVERSING WITH THE IDENTITY APPLICATIVE --
newtype Identity' a = Identity' {runIdentity' :: a}

instance Functor Identity' where
   fmap f (Identity' x) = Identity' (f x)

instance Applicative Identity' where
   pure = return
   af <*> ax = do
       f <- af
       x <- ax
       return (f x)

instance Monad Identity' where
   return x = Identity' x
   x >>= f = f (runIdentity' x)


   
-- What is
mystery1 :: (a -> b) -> a -> b
mystery1 f = runIdentity . fmap f . Identity
-- ?
-- Answer: mystery1 = id

-- What is 
mystery2 :: Traversable t => (a -> b) -> t a -> t b
mystery2 f = runIdentity . traverse (Identity . f)
-- ?
-- Answer: mystery2 = fmap

-- What is 
mystery3 :: a -> (a -> b) -> b
mystery3 x f = runIdentity (Identity x >>= (Identity . f))
-- ?
-- Answer: mystery3 = (&)   (that is, mystery3 x f = f x)
   





-- RELATING FOLDS AND TRAVERSALS -- 

newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
   fmap _ (Const x) = Const x

instance Monoid m => Applicative (Const m) where
   pure _ = Const mempty
   (<*>) (Const f) (Const b) = Const (f <> b)

foldMap'' :: Traversable t => Monoid m => (a -> m) -> t a -> m
foldMap'' f = getConst . sequenceA . fmap (Const . f)
