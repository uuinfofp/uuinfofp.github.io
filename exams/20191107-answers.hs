module Exam where
import Prelude hiding (log)
import System.Environment

--------------------------------------------------------------------------------
-- * 1. Functors and Monoids
--------------------
-- (a)
instance Functor First where
  fmap f (TheFirst x) = TheFirst (f x) -- 2
  fmap _ Never        = Never -- 2

-- (b)
instance Semigroup (First a) where
  Never          <> x     = x -- 1
  x@(TheFirst _) <> _     = x -- 1

instance Monoid (First a) where
  mempty = Never -- 2

-- (c)
bimapThese :: (l -> l') -> (r -> r') -> These l r -> These l' r'
bimapThese _ _ Neither = Neither -- 1
bimapThese f _ (This l) = This (f l) -- 1
bimapThese _ g (That r) = That (g r) -- 1
bimapThese f g (Both l r) = Both (f l) (g r) -- 1

-- (d)
class Bifunctor t where
   bimap :: (l -> l') -> (r -> r') -> t l r -> t l' r' -- 4

-- (e)
foo :: Bifunctor t => t Int Float -> t String String
foo = bimap show show -- 2 for show, 2 for bimap

--------------------------------------------------------------------------------
-- * 2. Testing
--------------------
-- (a)
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf []     _      = True -- 1
isPrefixOf (x:xs) []     = False -- 1
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys -- 2

-- (b)
before :: Eq a => [a] -> [a] -> [a] -> Bool
before xs ys zs = let i = findFirst xs zs -- 1
                      j = findFirst ys zs -- 1
                  in i <= j -- 2
                     && j < Never -- 2
-- OR

before xs ys zs = case (findFirst xs zs, findFirst ys zs) of -- 2
                    (Never     ,_)           -> False -- 1
                    (_         ,Never)       -> False -- 1
                    (TheFirst i, TheFirst j) -> i <= j -- 2

-- (c)
-- We could have concatImpl xs ys containing elements outside of xs and ys. -- 4
-- AND/OR
-- We could have concatImpl xs ys shorter than xs ++ ys in case xs is a sublist of ys. -- 4
-- AND/OR
-- We could have concatImpl xs ys containing multiple copies or xs and ys. -- 4

-- (d)
concatSpec                  :: Eq a => ([a] -> [a] -> [a])
                            -> [a] -> [a] -> Bool
concatSpec concatImpl xs ys = let zs = xs `concatImpl` ys
                              in before xs ys zs && length xs + length ys == length zs -- 4

--------------------------------------------------------------------------------
-- * 3. Monads
--------------------
-- (a)
combineLookup :: Ord k => (v -> v -> Maybe b) -> k -> k -> Map k v -> Maybe b
combineLookup f k1 k2 m = do v1 <- lookup k1 m -- 1
                             v2 <- lookup k2 m -- 1
                             f v1 v2 -- 2

-- (b)
main' = getArgs >>= \(fp:h:_) -> --1
         putStrLn h >>= \_ -> -- 1
           readFile fp >>= \s -> -- 1
             return (length s) -- 1

-- (c)
log   :: String -> Log ()
log m = MkLog [m] () -- 3, -1 if missing [ ], -2 if anything with IO

-- (d)
instance Monad Log where 
-- return :: a -> Log a
  return = MkLog [] -- 2
-- (>>=) :: Log a -> (a -> Log b) -> Log b
  MkLog ls x >>= k = let MkLog ls' y = k x -- 1
                     in MkLog (ls ++ ls') y -- 2

-- (e)
withoutLogging :: Log a -> IO a
withoutLogging (MkLog _ x) = return x -- 2

--------------------------------------------------------------------------------
-- * 4. Equational reasoning
--------------------
-- (a)

-- We prove the equation using structural induction on xs.

-- base case: xs = [] -- 3 in total

--   map f (foldr (\x r -> g x : r) [] [])
-- = { def foldr -- a } -- 1
--   map f []
-- = { def map -- c } -- 1
--   []
-- = { def map -- c } -- 1
--   map (f . g) []

-- induction step: xs = (x:xs') -- 9 in total
-- We assume the IH: map f (foldr (\x r -> g x : r) [] xs') = map (f . g) xs'. -- 2 

--   map f (foldr (\x r -> g x : r) [] (x:xs'))
-- = { def foldr -- b } -- 1
--   map f ((\x r -> g x : r) x (foldr (\x r -> g x : r) [] xs'))
-- = { function application  } -- 1
--   map f (g x : foldr (\x r -> g x : r) [] xs')
-- = { def map -- d } -- 1
--   f (g x) : map f (foldr (\x r -> g x : r) [] xs')
-- = { def . -- i } -- 1
--   (f . g) x : map f (foldr (\x r -> g x : r) [] xs')
-- = { IH } -- 2
--   (f . g) x : map (f . g) xs'
-- = { def map -- d } -- 1
--   map (f . g) (x:xs')


-- (b)
-- We prove the equation by using extensional reasoning.
-- That is, for all trees t we show that
-- size t = (length . toList) t
-- This, we show by structural induction on t.

-- base case: t = Leaf -- 2 in total

--   size Leaf
-- = { def size -- e } -- 1/2
--   0
-- = { def length } -- 1/2
--   length []
-- = { def toList -- g } -- 1/2
--   length (toList Leaf)
-- = { def . -- i } -- 1/2
--   (length . toList) Leaf


-- induction step: t = Node l x r -- 7 in total
-- We assume the IH: size t' = (length . toList) t', for t' in {l,r} -- 2

--   size (Node l x r)
-- = { def size -- f } 1/2    --- round up in case of half points
--   size l + 1 + size r
-- = { IH 2x } -- 2/2
--   (length . toList) l + 1 + (length . toList) r
-- = { def . 2x -- i } 1/2
--   length (toList l) + 1 + length (toList r)
-- = { def length } 1/2
--   length (toList l) + length [x] + length (toList r)
-- = { lemma -- j } 1/2
--   length (toList l ++ [x] ++ toList r)
-- = { def toList -- h } 1/2
--    length (toList (Node l x r))
-- = { def . -- i } 1/2
--    (length . toList) (Node l x r)


--------------------------------------------------------------------------------
-- * 5. Lazy evaluation
--------------------
-- (a)

-- WHNF (top level constructor :) 
-- -> False
-- WHNF (lambda expression)
-- -> e
-- WHNF (partial application of function)

-- 2 points for each: 1 for right answer, 1 for reason/reduction


-- (b)
-- A -- 4
-- B -- 1
-- C -- 1
-- D -- 0