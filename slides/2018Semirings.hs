-- let presEx = Matrix [[NoPath, Path 7 [("A","B")], Path 9 [("A","C")], NoPath, NoPath, NoPath], [NoPath, NoPath, NoPath, Path 7 [("B","D")], Path 4 [("B","E")], NoPath], [NoPath, NoPath, Path 3 [("C","B")], NoPath, Path 5 [("C","E")], NoPath], [NoPath, NoPath,NoPath,NoPath, NoPath, Path 10 [("D","F")]], [NoPath, NoPath, NoPath, Path 2 [("E","D")], NoPath, Path 2 [("E","F")]], [NoPath, NoPath, NoPath, NoPath, NoPath, NoPath]]

import Data.List 

infixl 9 @.
infixl 8 @+

class Semiring r where
    zero, one :: r
    closure :: r -> r
    (@+), (@.) :: r -> r -> r

instance Semiring Bool where
    zero = False
    one = True
    closure x = True
    (@+) = (||)
    (@.) = (&&)

instance Semiring Float where
    zero = 0
    one = 1
    closure 1 = undefined
    closure x = 1 / (1 - x)
    (@+) = (+)
    (@.) = (*)

data Matrix a = Scalar a | Matrix [[a]]
               deriving (Show)  

type BlockMatrix a = (Matrix a, Matrix a,
                      Matrix a, Matrix a)

data ShortestPath n = Path Int [(n,n)] | NoPath deriving (Show)

instance Ord n => Semiring (ShortestPath n) where
    zero = NoPath
    one = Path 0 []
    closure x = one

    x @+ NoPath = x
    NoPath @+ x = x
    Path a p @+ Path a' p' | a < a'            = Path a  p
                           | a == a' && p < p' = Path a  p
                           | otherwise         = Path a' p'

    x @. NoPath = NoPath
    NoPath @. x = NoPath
    Path a p @. Path a' p' = Path (a + a') (p ++ p')

mjoin :: BlockMatrix a -> Matrix a
mjoin (Matrix a, Matrix b,
       Matrix c, Matrix d) =
       Matrix ((a `hcat` b) ++ (c `hcat` d))
         where hcat = zipWith (++)

msplit :: Matrix a -> BlockMatrix a
msplit (Matrix (row:rows)) = (Matrix [[first]], Matrix [top], Matrix left, Matrix rest)
    where (first:top) = row
          (left, rest) = unzip (map (\(x:xs) -> ([x],xs)) rows)

instance Semiring a => Semiring (Matrix a) where
    zero = Scalar zero
    one = Scalar one

    Scalar a @+ Scalar b = Scalar (a @+ b)

    Matrix a @+ Matrix b = Matrix (zipWith (zipWith (@+)) a b)

    Scalar s @+ m = m @+ Scalar s
    Matrix [[a]] @+ Scalar b = Matrix [[a @+ b]]

    m @+ s = mjoin (first @+ s, top, left, rest @+ s)
      where (first, top, left, rest) = msplit m

    Scalar a @. Scalar b = Scalar (a @. b)
    Scalar a @. Matrix b = Matrix (map (map (a @.)) b)
    Matrix a @. Scalar b = Matrix (map (map (@. b)) a)
    Matrix a @. Matrix b = Matrix [[foldl1 (@+) (zipWith (@.) row col) | col <- cols] | row <- a]
      where cols = transpose b

    closure (Matrix [[x]]) = Matrix[[closure x]]
    closure m = mjoin (first' @+ top' @. rest' @. left', top' @. rest', rest' @. left', rest')
      where (first, top, left, rest) = msplit m
            first' = closure first
            top'   = first' @. top
            left'  = left @. first'
            rest'  = closure (rest @+ left' @. top)


