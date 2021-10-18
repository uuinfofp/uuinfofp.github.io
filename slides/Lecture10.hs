module Lecture10 where

  import System.Environment
  import System.Random
  import Control.Monad



-- do 
--     c <- getChar 
--     let c' = toUpper c
--     putChar c' 
-- -- gets converted (desugared) to 
-- getChar >>= (\c -> let c' = toUpper c in putChar c')






  -- type?
  mapMay :: (a->b) -> Maybe a -> Maybe b 
  mapMay f Nothing  = Nothing 
  mapMay f (Just a) = Just (f a)



  data Tree a = Leaf
              | Node (Tree a) a (Tree a) deriving Show
  
  --map :: (a->b) -> [a] -> [b]
  -- type?
  mapTree :: (a -> b) -> Tree a -> Tree b
  mapTree f Leaf = Leaf
  mapTree f (Node l v r) = Node (mapTree f l) (f v) (mapTree f r) 
  
  


  
  -- Functors as higher-kinded abstraction
  -- programs :: types :: kinds
  -- higher kinds
  
  -- True :: Bool 
  -- 4 :: Int 
  -- (3 + 4) :: Int


  -- Int :: *
  -- Bool :: *
  -- [] :: * -> *
  -- Maybe :: * -> *
  -- Tree :: * -> *
  -- [Int] :: *
  -- Maybe Bool :: *
  -- Map :: * -> * -> *

  type MyMap = Map String Int 
  

  -- in the ghci, you can check types by 
  -- :t expr

  -- similarly, check kinds by
  -- :k mytype

  -- :k Int
  -- > *



  data HigherOrderTypeFormer f a = Val a (f a)
  -- HigherOrderTypeFormer :: ...
  -- any functor :: ...
  

-- (->) :: * -> * -> *
-- (->) Int :: * -> * 
-- (->) Int Bool = Int -> Bool :: * 
  
  instance Functor ((->) r) where -- (->)
    -- type?
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
    -- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
    fmap f g = f . g

-- (->) Bool a = (a, a)
-- (->) Int a = (a, ...., a)
-- (->) r a = r-indexed family of as
  
  
  instance Functor IO where 
  -- type?
  -- fmap :: (a->b) -> IO a -> IO b
      fmap f ia = do 
          a <- ia
          return (f a)

-- toUpper :: Char -> Char 
-- getChar :: IO Char 
-- toUpper <$> getChar :: IO Char
  


  -- map/Functor intuition: keep shape of container the same but transform the values it holds by applying function (in the sense that functor laws hold).
  
  

notFMap :: (a -> b) -> [a] -> [b]
notFMap f [] = [] 
notFMap f (x:xs) = []

-- fmap id xs = id xs = xs 
-- notFMap id [1,2] = []


fmap :: Monad m => (a -> b) -> m a -> m b 
fmap f ma = ma >>= (\a -> return (f a))


  -- Recall that IO represents computation with io-side effects.
  



  -- What notion of computation does Maybe represent?
  
  



  -- How is each monad a functor?
  fmap :: Monad m => (a -> b) -> m a -> m b 
  fmap = undefined
  
  
  
  f :: Maybe Int -> Maybe Int
  f m = do
      x <- m
      return 3
      return (x + 1)
-- is equivalent to --   
  f' m = do
      x <- m
      _ <- Nothing
      return (x + 1)
-- is equivalent to --  
  f'' m = Nothing



Nothing >>= _ = Nothing 
Just x >>= f = f x






  fNothing m = do 
      _ <- Nothing 
      x <- m
      return (x + 1)
-- is equivalent to --
  fNothing' m = undefined
-- is equivalent to --
  fNothing'' m = undefined
-- is equivalent to -- 
  fNothing2 m = undefined
-- is equivalent to -- 
  fNothing3 m = undefined
          
  g :: Maybe Int -> Maybe Int
  g m = do
      x <- return 3
      y <- m
      return (x + y)
  
  g' :: Maybe Int -> Maybe Int
  g' m = do
      x <- Just 3
      y <- m
      return (x + y)


  h :: Maybe Int -> IO Int -> Maybe Int
  h x y = do 
      x' <- x 
      y' <- y'
      return (x'+ y')


-- Any monad m 
----------------
-- return :: a -> m a 
-- (>>=) :: m a -> (a -> m b) -> m b
-- so, for any monad, we have do notation




-- For IO 
-----------
-- getChar :: IO Char 
-- putChar :: Char -> IO () 
-- etc. 



-- For Maybe 
-------------
-- Nothing :: Maybe a




-- For List
-------------
-- [] :: [a]
-- (++) :: [a] -> [a] -> [a]




  
  instance Monad [] where 
      -- return :: a -> [a]
      return a = [a]
      -- (>>=) :: [a] -> (a -> [b]) -> [b]
      as >>= f = concatMap f as
  

  
  mzero' :: Maybe a 
  mzero' = Nothing 
  mplus' j@(Just a) _ = j
  mplus' _ b = b 
  
  -- What notion of computation does List represent?
  
  filter' :: MonadPlus m => (a -> Bool) -> m a -> m a 
  filter' p ma = undefined
  
  filter'' p ma = undefined


-- do 
--     x <- [1,2,3]
--     y <- [4,5,6]
--     return x+y
-- -- equivalent to 
-- [1,2,3] >>= (\x -> [4,5,6] >>= (\y -> return (x+y)))
-- -- equivalent to 
-- [1,2,3] >>= (\x -> [4,5,6] >>= (\y -> [x+y]))
-- -- equivalent to 
-- concatMap (\x -> [4,5,6] >>= (\y -> [x+y])) [1,2,3]
-- -- equivalent to 
-- concatMap (\x -> concatMap (\y -> [x+y]) [4,5,6]) [1,2,3]
-- -- equivalent to 
-- concatMap (\x -> [x+4,x+5,x+6]) [1,2,3]
-- -- equivalent to 
-- concat [[1+4,1+5,1+6], [2+4,2+5,2+6], [3+4,3+5, 3+6]]
-- -- equivalent to 
-- [1+4,1+5,1+6, 2+4,2+5,2+6, 3+4,3+5, 3+6]