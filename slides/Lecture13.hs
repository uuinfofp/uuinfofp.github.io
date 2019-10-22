module Lecture13 where

data ArithOp
  = Plus
  | Minus
  | Times
  | Div

data Instr
  = Number Float
  | Operation ArithOp

type RPN = [Instr]

type Stack = [Float]

evalOp :: ArithOp -> Float -> Float -> Float
evalOp Plus  = (+)
evalOp Minus = (-)
evalOp Times = (*)
evalOp Div   = (/)

evalRPN :: RPN -> Maybe Float
evalRPN rpn =
  let initialStack = []
      stackTranss = map evalInstr' rpn
      stackTrans = foldr (.) id stackTranss
      finalStack = stackTrans initialStack
   in (case finalStack of
        [x] -> Just x
        _  -> Nothing)

evalInstr' :: Instr -> Stack -> Stack
evalInstr' (Number f) stack = push' f stack
evalInstr' (Operation op) stack =
  let (x, stack') = pop stack
   in let (y, stack'') = pop stack'
       in push' (evalOp op x y) stack''

pop :: Stack -> (Float, Stack)
pop (x:xs) = (x, xs)

push' :: Float -> Stack -> Stack
push' x xs = x : xs

evalInstr :: Instr -> Stack -> Stack
evalInstr (Number f) stack = push' f stack
evalInstr (Operation op) stack =
  let (x, stack') = pop stack
   in let (y, stack'') = pop stack'
       in push' (evalOp op x y) stack''

push :: Float -> Stack -> ((), Stack)
push x xs = ((), x : xs)

next ::
     (Stack -> (a, Stack))
  -> (a -> Stack -> (b, Stack))
  -> (Stack -> (b, Stack))
next transA transB s =
  let (a, s') = transA s
   in transB a s

evalInstr'' :: Instr -> Stack -> ((), Stack)
evalInstr'' (Number f) = push f
evalInstr'' (Operation op) =
  pop `next` \x -> pop `next` \y -> push (evalOp op x y)

return' :: a -> Stack -> (a, Stack)
return' a s = (a, s)

evalInstr_ :: Instr -> State Stack ()
evalInstr_ (Number f) = S (push f)
evalInstr_ (Operation op) = do
  x <- S pop
  y <- S pop
  S (push (evalOp op x y))

run :: State s a -> s -> a
run (S f) s =
  let (a, _) = f s
   in a

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)

label :: Tree a -> Tree (Int, a)
label t = run (label' t) 0

getState :: State a a
getState = S (\a -> (a, a))

increment :: State Int ()
increment = S (\i -> ((), i + 1))

nextLabel = increment >>= \_ -> getState

label' :: Tree a -> State Int (Tree (Int, a))
label' Leaf = return Leaf
label' (Node l v r) = do
  l' <- label' l
  r' <- label' r
  i <- nextLabel
  return (Node l' (i, v) r')

miffy :: State Stack ()
miffy = do
  x <- S pop
  if x == 3
    then S (push 7)
    else pure ()

aiffy :: State Stack ()
aiffy = pure cond <*> S pop <*> S (push 7) <*> pure ()
  where
    cond x y z =
      if x == 3
        then y
        else z

newtype Phantom m a =
  Phantom
    { get :: m
    }

instance Monoid m => Functor (Phantom m) where
  fmap _ p = Phantom (get p)

instance Monoid m => Applicative (Phantom m) where
  pure _ = Phantom mempty
  f <*> p = Phantom (get f <> get p)

data Writer m a =
  Writer m a

instance Functor (Writer m) where
  fmap f (Writer m a) = Writer m (f a)

instance Monoid m => Applicative (Writer m) where
  pure = Writer mempty
  (Writer m1 f) <*> (Writer m2 a) = Writer (mappend m1 m2) (f a)

instance Monoid m => Monad (Writer m) where
  (Writer m1 a) >>= f =
    case f a of
      Writer m2 b -> Writer (m1 <> m2) b

data Error m a
  = Error m
  | OK a

instance Functor (Error m) where
  fmap f (OK a)    = OK (f a)
  fmap f (Error m) = Error m

liftM2'' :: Monoid m => (a -> b -> c) -> Error m a -> Error m b -> Error m c
liftM2'' _ (Error m1) (Error m2) = Error (m1 <> m2)
liftM2'' _ (OK a) (Error m)      = Error m
liftM2'' _ (Error m) (OK b)      = Error m
liftM2'' f (OK a) (OK b)         = OK (f a b)

instance Monoid m => Applicative (Error m) where
  pure = OK
  (Error m1) <*> (Error m2) = Error (m1 <> m2)
  (Error m1) <*> (OK a) = Error m1
  (OK f) <*> (Error m2) = Error m2
  (OK f) <*> (OK a) = OK (f a)

instance Monoid m => Monad (Error m) -- Induces different applicative structure!! Error propagation, rather than error accumulation.
                                                                                                                                     where
  (OK a) >>= f = f a
  (Error m) >>= f = Error m

(<**>) :: Monad m => m (a -> b) -> m a -> m b
mf <**> ma = mf >>= \f -> ma >>= \a -> return (f a)

liftM1' f a = pure f <*> a

liftM2' f a b = pure f <*> a <*> b

liftM3' f a b c = pure f <*> a <*> b <*> c

newtype Reader a b =
  Reader (a -> b)

instance Functor (Reader a) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader a) where
  pure b = Reader (\_ -> b)
  (Reader f) <*> (Reader b) = Reader (\a -> f a (b a))

instance Monad (Reader a) where
  (Reader b) >>= f =
    Reader
      (\a ->
         case f (b a) of
           Reader g -> g a)

newtype State s a =
  S (s -> (a, s))

instance Functor (State a) where
  fmap f (S transState) =
    S
      (\a ->
         let (b, a) = transState a
          in (f b, a))

instance Applicative (State a) where
  pure b = S (\a -> (b, a))
  (S f) <*> (S b) =
    S
      (\a ->
         let (f', a') = f a
          in let (b', a'') = b a'
              in (f' b', a''))

instance Monad (State a) where
  (S b) >>= f =
    S
      (\a ->
         let (b', a') = b a
          in case f b' of
               S f' -> f' a')
