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

evalRPN :: RPN -> Float
evalRPN is = case foldl op initialStack is of 
    [f] -> f
    _ -> error "Final stack was not a singleton!"
    where
        op = flip evalInstr'
        initialStack = []



evalInstr' :: Instr -> Stack -> Stack
evalInstr' (Number f) fs = f : fs
evalInstr' (Operation o) (f1 : f2 : fs) = evalOp o f1 f2 : fs
evalInstr' _ _ = error "Stack with too few elements!" 















--  :: State Float
pop :: Stack -> (Float, Stack)
pop (f : fs) = (f, fs)

push' :: Float -> Stack -> Stack
push' = (:)

evalInstr :: Instr -> Stack -> Stack
evalInstr (Number f) fs = push' f fs
evalInstr (Operation o) fs = let (f1, fs') = pop fs in 
                             let (f2, fs'') = pop fs' in
                             let f3 = evalOp o f1 f2 in 
                                 push f3 fs''


--      Float -> State ()
push :: Float -> Stack -> ((), Stack)
push f fs = ((), f : fs)







--   :: 
--- State a -> (a -> State b) -> State b
next ::
     (Stack -> (a, Stack))
  -> (a -> Stack -> (b, Stack))
  -> Stack -> (b, Stack)
next f g s = let (a, s') = f s in g a s'

evalInstr'' :: Instr -> Stack -> ((), Stack)
evalInstr'' (Operation o) = pop `next` \f1 -> 
                            pop `next` \f2 -> 
                                push (evalOp o f1 f2)









--      :: a -> State a
return' :: a -> Stack -> (a, Stack)
return' a s = (a, s)

evalInstr_ :: Instr -> State Stack ()
evalInstr_ = undefined


















run :: State s a -> s -> a
run (S f) s = fst (f s)











data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)






newtype State s a = S (s -> (a, s))

label :: Tree a -> Tree (Int, a)
label t = run (label' t) 0

{-
         -------------a----- return value
        |         |
----a-----------------a-----  state 
-}

getState :: State a a
getState = S (\a -> (a, a))



{-
         ---------------()---------
        |         |
----Int-----------------Int----- state 
-}
increment :: State Int ()
increment = S (\i -> ((), i + 1))

-- (3, 2.4) :: (Int, Float)
-- (True, 4, 35.334) :: (Bool, Int, Float)
-- (False) :: (Bool)
-- () :: ()


nextLabel :: State Int Int
nextLabel = undefined

label' :: Tree a -> State Int (Tree (Int, a))
label' Leaf = return Leaf  -- Leaf :: Tree (Int, a), return :: a -> State s a
label' (Node l v r) = do 
    l' <- label' l 
    r' <- label' r
    c <- getState 
    increment 
    return (Node l' (c, v) r')

label' (Node l v r) = 
    label' l >>= \l' -> 
        label' r >>= \r' -> 
            getState >>= \c -> 
                increment >>= \() -> 
                    return (Node l' (c,v) r')




get :: State s s  -- reading state
get = getState 

put :: s -> State s ()  -- writing state
put s = S (\_ -> ((), s))

modify :: (s -> s) -> State s ()  -- defined in terms of get and put
--modify f = S (\s -> ((), f s))
modify f = do 
    s <- getState 
    put (f s)


{-
Effect of >=>:

--------------------
       |   f   |
--------------------


--------------------
       |   g   |
--------------------

-------------produces---------------------------------

----------------       --------------------------------
      f>=> g                     | f |        | g | 
----------------       --------------------------------

-}



(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) f g a  = do 
    b <- f a
    g b

--- Monad laws:
{-
return >=> f = f -- left unit

g >=> return = g  -- right unit

(f >=> g) >=> h = f >=> (g >=> h) -- associativity
-}










-- fmap :: Functor f => (a -> b) -> f a -> f b
liftM2_ :: Monad m => (a -> b -> c) -> (m a -> m b -> m c)
liftM2_ f ma mb = do 
    a <- ma 
    b <- mb 
    return (f a b)























ppure :: Monad m => a -> m a 
ppure = return

(<**>) :: Monad m => m (a -> b) -> m a -> m b
(<**>) mf ma = do 
    f <- mf 
    a <- ma 
    return (f a)


liftM0 :: Monad m => a -> m a
liftM0 = return 


























miffy :: State Stack ()
miffy = do
  x <- S pop
  if x == 3
    then S (push 7)
    else pure ()
  
miffy' = S pop >>= (\x -> if x == 3 then S (push 7) else pure ())

aiffy :: State Stack ()
aiffy = cond <$> S pop <*> S (push 7) <*> pure ()
  where
    cond x y z =
      if x == 3
        then y
        else z

aiffy' :: State Stack ()
aiffy' = (\x y z -> if x == 3 then y else z) <$> S pop <*> S (push 7) <*> pure ()
aiffy'' = S pop >>= (\x -> S (push 7) >>= (\y -> pure (if x == 3 then y else ())))























data Error m a
  = Error m
  | OK a

instance Functor (Error m) where
  fmap = undefined

liftM2'' :: Monoid m => (a -> b -> c) -> Error m a -> Error m b -> Error m c
liftM2'' = undefined

instance Monoid m => Applicative (Error m) where
  pure = undefined
  (<*>) = undefined

instance Monoid m => Monad (Error m) -- Induces different applicative structure!! Error propagation, rather than error accumulation.
                                                                                                                                     where
  (>>=) = undefined
  

















  



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


-- type State s a = s -> (a, s)
instance Monad (State s) where 
    -- return :: a -> State s a "= a -> s -> (a, s)""
    return a = S (\s -> (a, s))
    -- (>>=) :: State s a -> (a -> State s b) -> State s b "= (s -> (a, s)) -> (a -> s -> (b, s)) -> (s -> (b, s)) "
    (>>=) (S f) g = S (\s -> let (a,s') = f s in let S h = g a in h s') 







liftA1 :: Applicative f => (a -> b) -> f a -> f b
liftA1 g fa = (pure g) <*> fa 

liftA2  :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
liftA2 g fa fb = (pure g) <*> fa <*> fb

liftA3  :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 g fa fb fc = (pure g) <*> fa <*> fb <*> fc