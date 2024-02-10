data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf a) = Leaf (g a)
  fmap g (Node a b) = Node (fmap g a) (fmap g b)

-- instance Functor Maybe where
--     -- fmap :: (a -> b) -> Maybe a -> Maybe b
--     fmap g Nothing  = Nothing
--     fmap g (Just x) = Just (g x)

-- class Functor f => Applicative f where
--     pure :: a -> f a
--     (<*>) :: f(a -> b) -> f a -> f b

-- instance Applicative Maybe where
--     -- pure :: (a) -> Maybe a
--     pure a = Just a

--     -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--      (Just g) (<*>) mx = fmap g mx
--      _ (<*>) _         = Nothing

-- instance Applicative [] where
--     -- pure :: a -> [a]
--     pure x = [x]

--     --(<*>) :: [(a-> b)] -> [a] -> [b]j
--     gs (<*>) xs = [g x | g <- gs, x <- xs]

inc :: (Functor f) => f Int -> f Int
inc = fmap (+ 1)

fmap2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = (g <$> x) <*> y

data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

-- eval :: Expr -> Maybe Int
-- eval (Val n) = Just n
-- eval (Div x y) = pure (safediv) <*> eval x <*> eval y

eval :: Expr -> Maybe Int
eval (Val n) = Just n
-- eval (Div x y) = eval x >>= (\m -> eval y >>= safediv m)

eval (Div x y) = do
  m <- eval x
  n <- eval y
  safediv m n

pairs :: (Monad f) => f a -> f b -> f (a, b)
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

-- g' s = (b, s')
--   where
--    (a, s') = g
--     b = f a

-- pairs xs ys = xs >>= (\x -> ys >>= (\y -> return (x, y)))
-- pairs xs ys = [(x, y) | x <- xs, y <- ys]

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f st = S (\s -> let (a, s') = app st s in (f a, s'))

instance Applicative ST where
  -- pure :: a -> ST a
  pure a = S (\s -> (a, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  (<*>) :: ST (a -> b) -> ST a -> ST b
  stl <*> str =
    S
      ( \s ->
          let (f, s') = app stl s
              (a, s'') = app str s'
           in (f a, s'')
      )

instance Monad ST where
  -- (>>=) ST a -> (a -> ST b) -> ST b
  st >>= f =
    S
      ( \s ->
          let (a, s') = app st s
              (b, s'') = app (f a) s'
           in (b, s'')
      )

rlabel :: Tree a -> ST (Tree Int)
-- rlabel :: Tree a -> Int -> (Tree a, Int)
-- S (Int -> (Int, Int))

rlabel (Leaf x) = S (\s -> (Leaf s, s + 1))
rlabel (Node l r) = do
  l' <- rlabel l
  r' <- rlabel r
  return (Node l' r')

-- rlabel (Node l r) = rlabel l >>= (\l' -> rlabel r >>= (\r' -> return (Node l' r')))

label :: Tree a -> Tree Int
label t = fst (app (rlabel t) 0)
