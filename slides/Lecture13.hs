module Lecture13Answers where

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
evalOp Plus = (+)
evalOp Times = (*)
evalOp Minus = (-)
evalOp Div = (/)

evalRPN :: RPN -> Maybe Float
evalRPN rpn = let initialStack = [] 
                  stackTranss = map evalInstr rpn 
                  stackTrans = foldr (flip (.)) id stackTranss
                  finalStack = stackTrans initialStack in 
                    case finalStack of
                      [i] -> Just i
                      _ -> Nothing


evalInstr' :: Instr -> Stack -> Stack
evalInstr' (Number i) is = i : is
evalInstr' (Operation op) (i : j : is) = evalOp op i j : is







pop :: Stack -> (Float, Stack)
pop [] = error "Cannot pop an empty stack"
pop (f : fs) = (f, fs)

push' :: Float -> Stack -> Stack
push' f fs = f : fs

evalInstr :: Instr -> Stack -> Stack
evalInstr (Number i) is = push' i is
evalInstr (Operation op) s = let (i1, is) = pop s 
                                 (i2, is') = pop is in 
                                   push' (evalOp op i1 i2) is'
 
push :: Float -> Stack -> ((), Stack)
push f fs = ((), f : fs)



next ::
     (Stack -> (a, Stack))
  -> (a -> Stack -> (b, Stack))
  -> (Stack -> (b, Stack))
next command1 condCommand2 s = let (a, s') = command1 s in condCommand2 a s' 


evalInstr'' :: Instr -> Stack -> ((), Stack)
evalInstr'' (Number i) = push i
evalInstr'' (Operation op) = pop `next` \i1 ->
                             pop `next` \i2 ->
                             push (evalOp op i1 i2)


return' :: a -> Stack -> (a, Stack)
return' a s = (a, s)

evalInstr_ :: Instr -> State Stack ()
evalInstr_ (Number i) = (S . push) i 
evalInstr_ (Operation op) = do 
  i1 <- S pop 
  i2 <- S pop 
  (S . push) (evalOp op i1 i2)

run :: State s a -> s -> a
run (S f) s = let (a, _) = f s in a


nextt :: (a -> State s b) -> (b -> State s c) -> (a -> State s c)
nextt f g a = do 
  b <- f a
  g b



data Tree a = Leaf
            | Node (Tree a) a (Tree a)




label :: Tree a -> Tree (Int, a)
label t = run (label' t) 0

getState :: State a a
getState = S (\s -> (s, s)) 

increment :: State Int ()
increment = S (\i -> ((), i + 1))

nextLabel :: State Int Int
nextLabel = do
  increment 
  getState

label' :: Tree a -> State Int (Tree (Int, a))
label' Leaf = return Leaf 
label' (Node lt v rt) = do
  lt' <- label' lt
  rt' <- label' rt
  i <- nextLabel
  return (Node lt' (i, v) rt')


liftM2_ :: Monad m => (a -> b -> c -> d) -> (m a -> m b -> m c -> m d)
liftM2_ f ma mb mc  = do 
  a <- ma 
  b <- mb
  c <- mc
  return (f a b c)



(<**>) :: Monad m => m (a -> b) -> m a -> m b
mf <**> ma = mf >>= \f -> ma >>= \a -> return (f a)

ffmap :: Applicative f => (a -> b) -> f a -> f b 
ffmap f ma = (pure f) <*> ma



-- Example of behaviour you can achieve using the state monad
-- that you cannot realise using only the state applicative
miffy :: State Stack ()
miffy = do
  x <- S pop
  if x == 3
    then S (push 7)
    else pure ()
  
miffy' = (S pop) >>= (\x -> if x == 3 then S (push 7) else pure ())

-- This is the best approximation that you can achieve 
-- with the state applicative
aiffy :: State Stack ()
aiffy = pure cond <*> S pop <*> S (push 7) <*> pure ()
  where
    cond x y z =
      if x == 3
        then y
        else z

aiffy' :: State Stack ()
aiffy' = pure (\x y z -> if x == 3 then y else z) <*> S pop <*> S (push 7) <*> pure ()
aiffy'' = (S pop) >>= (\x -> S (push 7) >>= (\y -> pure (if x == 3 then y else ())))
-- The difference is that the applicative always 
-- pushes 7 onto the stack, even if x is not equal to 3.













-- Error accumulation, an example of an applicative that does not arise from a monad
data Error m a
  = Error m
  | OK a

instance Functor (Error m) where
  fmap f (OK a) = OK (f a)
  fmap f (Error m) = Error m

liftM2'' :: Monoid m => (a -> b -> c) -> Error m a -> Error m b -> Error m c
liftM2'' f (OK a1) (OK a2) = OK (f a1 a2)
liftM2'' f (OK _) (Error m) = Error m
liftM2'' f (Error m) (OK _) = Error m 
liftM2'' f (Error m1) (Error m2) = Error (m1 <> m2) 

instance Monoid m => Applicative (Error m) where
  pure = OK
  (OK f) <*> (OK a) = OK (f a)
  (Error m) <*> (OK _) = Error m 
  (OK _) <*> (Error m) = Error m 
  Error m1 <*> Error m2 = Error (m1 <> m2)

instance Monoid m => Monad (Error m) -- Induces different applicative structure!! Error propagation, rather than error accumulation.
                                                                                                                                     where
  OK a >>= f = f a 
  Error m >>= f = Error m
  
























liftM1' f a = pure f <*> a

liftM2' f a b = pure f <*> a <*> b

liftM3' f a b c = pure f <*> a <*> b <*> c



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
  mf <*> mb = do 
    f <- mf 
    b <- mb 
    return (f b)
  -- (S f) <*> (S b) =
    -- S
    --   (\a ->
    --      let (f', a') = f a
    --       in let (b', a'') = b a'
    --           in (f' b', a''))

instance Monad (State a) where
  (S b) >>= f =
    S
      (\a ->
         let (b', a') = b a
          in case f b' of
               S f' -> f' a')
