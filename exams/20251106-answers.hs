module Exam20251106 where

import Prelude hiding (lookup)
import qualified Data.Map as M

type Map k v = M.Map k v

lookup :: Ord k => k -> Map k v -> Maybe v
lookup = M.lookup

fromList :: Ord k => [(k, v)] -> Map k v
fromList = M.fromList


--------------------------------------------------------------------------------
-- Question 1 – Equational reasoning on trees
--------------------------------------------------------------------------------

data Tree = Leaf
          | Node Tree Tree
          deriving (Eq, Show)

mirror :: Tree -> Tree
mirror Leaf       = Leaf        -- (a)
mirror (Node l r) = Node (mirror r) (mirror l)  -- (b)

{-
We also use function composition:

  (.) :: (b -> c) -> (a -> b) -> (a -> c)
  (f . g) x = f (g x)                                 -- (c)

Claim: (mirror . mirror) t = t  for all trees t :: Tree.

Proof:

We prove the claim by structural induction on t.

Base case: t = Leaf.

  (mirror . mirror) Leaf
  = -- by (c)
    mirror (mirror Leaf)
  = -- by (a)
    mirror Leaf
  = -- by (a)
    Leaf

So the claim holds for t = Leaf.

Inductive case: t = Node l r.

We assume the induction hypotheses:
  I.H.1: (mirror . mirror) l = l
  I.H.2: (mirror . mirror) r = r

We must show that (mirror . mirror) (Node l r) = Node l r.

  (mirror . mirror) (Node l r)
  = -- by (c)
    mirror (mirror (Node l r))
  = -- by (b)
    mirror (Node (mirror r) (mirror l))
  = -- by (b)
    Node (mirror (mirror l)) (mirror (mirror r))
  = -- by (c)
    Node ((mirror . mirror) l) (mirror (mirror r))
  = -- by (c)
    Node ((mirror . mirror) l) ((mirror . mirror) r)
  = -- by I.H.1
    Node l ((mirror . mirror) r)
  = -- by I.H.2
    Node l r

Thus the claim holds for t = Node l r assuming the induction hypotheses.

By induction on t, we conclude that (mirror . mirror) t = t for all trees t :: Tree.
-}


--------------------------------------------------------------------------------
-- Question 2 – Programming on a simple HTML datatype
--------------------------------------------------------------------------------

data Tag
  = Div          -- container
  | P            -- paragraph
  | H Int        -- H1 .. H6 tags
  | Span         -- inline
  | Img String   -- image with the source URL
  | Br           -- newline
  deriving (Show, Eq)

type AttributeName = String

data HtmlP v
  = TextNode String
  | Elem Tag (Map AttributeName v) [HtmlP v]
  deriving (Show, Eq)

type Html = HtmlP String

myHtml :: Html
myHtml =
  Elem Div (fromList [])
    [ Elem (Img "header.jpg") (fromList []) []
    , Elem (H 1) (fromList [("id","myTitle")])
        [TextNode "My FP Webpage"]
    , TextNode "FP is "
    , Elem Span (fromList [("class","emphasize")])
        [TextNode "Wonderful!"]
    , Elem Div (fromList [("id","bar")])
        [ Elem (Img "lambda.jpg") (fromList [("id","lambda")]) []
        , Elem Div (fromList [("id","empty")]) []
        ]
    ]

-------------------------------------------------------------------------------
-- 2(a) collectIds
-------------------------------------------------------------------------------

collectIds :: Html -> [String]
collectIds (Elem Div ats chs) = lookupId ats ++ concatMap collectIds chs
collectIds _       = []

lookupId :: Map AttributeName String -> [String]
lookupId ats =
  case lookup "id" ats of
    Just v  -> [v]
    Nothing -> []


-------------------------------------------------------------------------------
-- 2(b) replaceImages
-------------------------------------------------------------------------------

replaceImages :: Html -> Html
replaceImages (Elem (Img url) _ []) = TextNode ("image: " <> url) 
replaceImages (Elem e ats chs) = Elem e ats (map replaceImages chs) 
replaceImages other = other

-------------------------------------------------------------------------------
-- 2(c) verifySpans
-------------------------------------------------------------------------------

verifySpans :: Html -> Bool
verifySpans (Elem Span _ chs) = all validSpanChildren chs
verifySpans (Elem _ _ chs) = all verifySpans chs
verifySpans _ = True 


-- A direct child of a <span> must be a TextNode.
validSpanChildren :: Html -> Bool
validSpanChildren (TextNode _) = True
validSpanChildren _ = False

-------------------------------------------------------------------------------
-- 2(d) Functor instance for HtmlP
-------------------------------------------------------------------------------

instance Functor HtmlP where
    fmap f (Elem e m l) = Elem e (fmap f m) (map (fmap f) l) 
    fmap f (TextNode s)= TextNode s


--------------------------------------------------------------------------------
-- Question 3 – IO and monads
--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- 3(a) promptUntil
-------------------------------------------------------------------------------

promptUntil :: (String -> Bool) -> IO [String]
promptUntil p = do
  x <- getLine
  if not (p x)
    then do
      xs <- promptUntil p
      return (x : xs)
    else
      return []


-------------------------------------------------------------------------------
-- 3(b) sumInt in bind notation
-------------------------------------------------------------------------------

sumInt :: IO Int
sumInt =
  promptUntil null >>= \ss ->
    let n = sum (map read ss)
    in print n >>= \_ ->
       return n


--------------------------------------------------------------------------------
-- Question 4 – Specification and testing
--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- 4(a) chunksOf via unfoldr
-------------------------------------------------------------------------------

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr f s =
  case f s of
    Nothing      -> []
    Just (a, s') -> a : unfoldr f s'

-- Precondition (conceptually): n > 0.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n = unfoldr step
  where
    step [] = Nothing
    step xs = Just (splitAt n xs)

-------------------------------------------------------------------------------
-- 4(b) spec
-------------------------------------------------------------------------------
{-
In words, for n > 0 and any list xs, spec should check that:

  1. No elements are lost or reordered:
       concat (chunksOf n xs) == xs

  2. Every chunk except possibly the last one has length exactly n.

  3. If there is a last chunk, its length is at most n
     (and when xs is empty there are no chunks at all).

Such a spec can then be used with QuickCheck to test that our
implementation of chunksOf behaves correctly.
-}

-------------------------------------------------------------------------------
-- 4(c) Type of spec
-------------------------------------------------------------------------------

spec :: (Eq a, Arbitrary a) => [a] -> Int -> Bool
spec xs n = undefined


--------------------------------------------------------------------------------
-- Question 5 – Lazy evaluation and WHNF
--------------------------------------------------------------------------------

-- We only record the answers (WHNF results); the reasoning is done on paper.
--
-- 5(a) undefined
-- 5(b) 0
-- 5(c) True
-- 5(d) undefined
-- 5(e) 99
-- 5(f) the given expression is already in WHNF, so it stays as-is
--
-- Bonus:
-- 5(g) False
-- 5(h) undefined


--------------------------------------------------------------------------------
-- Question 6 – Run-length encoding with a fold
--------------------------------------------------------------------------------

rle :: Eq a => [a] -> [(a, Int)]
rle = foldr f e
  where
    e = []
    f a [] = [(a, 1)]
    f a l@((a', i) : rest)
      | a == a'  = (a, i + 1) : rest
      | otherwise = (a, 1) : l




--------------------------------------------------------------------------------
-- Make the module runnable as an executable 
--------------------------------------------------------------------------------

main :: IO ()
main = pure ()
