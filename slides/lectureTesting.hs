{- cabal:
build-depends: base, QuickCheck
-}
import Test.QuickCheck

-- load in ghci with: 'cabal repl <thisfilename>.hs' then run 'main'


--------------------------------------------------------------------------------
-- Generating Sorted lists and running quickcheck

-- The alternative to using the 'forAll' combinator is to use a newtype around [Int]:

newtype SortedIntList = SortedIntList [Int]
  deriving (Show)

-- generate only sorted list of ints
instance Arbitrary SortedIntList where
  arbitrary = genSorted
    where
      genSorted = undefined  -- see slides

insert = undefined -- see slides

isSorted = undefined -- see slides


insertCorrectProp                      :: Int -> SortedIntList -> Bool
insertCorrectProp x (SortedIntList xs) = isSorted (insert x xs)

checkInsert :: IO ()
checkInsert = quickCheck insertCorrectProp

--------------------------------------------------------------------------------
-- Exercise generating arbitrary trees

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show,Eq)

-- recall that:

-- class Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b

-- instance Monad Gen

-- class Arbitrary a where
--   arbitrary :: Gen a

-- goal: Make 'Tree a' an instance of arbitrary

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = arbitraryTree

arbitraryTree :: Arbitrary a => Gen (Tree a)
arbitraryTree = frequency listOfGens
    where
      -- listOfGens :: [(Int, Gen (Tree a)) ]
      listOfGens = [ ( 2 , genLeaf )
                   , ( 1 , genNode )
                   ]
      -- genLeaf :: Gen (Tree a)
      genLeaf = return Leaf

      -- genNode :: Gen (Tree a)
      genNode = do x <- arbitrary  -- (x :: a)
                   l <- arbitraryTree -- we could simply have written 'arbitrary' here as well
                   r <- arbitraryTree
                   return $ Node l x r

      -- alternatively we can write genNode as:

      -- genNode = Node <$> arbitraryTree
      --                <*> arbitrary
      --                <*> arbitraryTree

----------------------------------------

size              :: Tree a -> Int
size Leaf         = 0
size (Node l _ r) = 1 + size l + size r



elems              :: Tree a -> [a]
elems Leaf         = []
elems (Node l x r) = elems l ++ [x] ++ elems r


-- | Property that checks that elems preserves size
elemsPreservesSize           :: (Tree a -> [a])
                             -> Tree a
                             -> Bool
elemsPreservesSize myElems t = length (myElems t) == size t


-- this prints 10 arbitrary (= randomly generated) 'Tree Int' s
main = do putStrLn "print 10 arbitrary 'Tree Int's"
          putStrLn "----------------------------------------"
          sample (arbitrary :: Gen (Tree Int))
          let elems' = elems :: Tree Int -> [Int]
          putStrLn "----------------------------------------"
          putStrLn "check that elems preserves size"
          quickCheck $ elemsPreservesSize elems'
