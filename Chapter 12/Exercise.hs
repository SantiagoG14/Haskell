-- instance Functor ((->) a)

-- -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
-- fmap = (.)

-- instance Applicative ((->) a)

-- -- pure :: b -> (a -> b)
-- pure = const

-- -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
-- f <*> g = \y -> f y (g y)

newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  -- fmap (a -> b) -> ZipList a -> ZipList b
  fmap f (Z xs) = Z (map f xs)

instance Applicative ZipList where
  -- pure a -> ZipList a
  pure x = Z (repeat x)

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (x, g) <- zip xs gs]

-- instance Monad ((->) a) where
--   -- (>>=) (a -> b) -> (b -> a -> c) -> (a -> c)
--   f >>= g = \y -> g (f y) y

--   -- return :: b -> a -> b
--   return = pure

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  -- fmap (a -> b) -> Expr a -> Expr b
  fmap g (Var x) = Var (g x)
  fmap g (Val n) = Val n
  fmap g (Add l r) = Add (fmap g l) (fmap g r)

instance Applicative Expr where
  -- pure a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b

  (Var f) <*> (Var x) = Var (f x)
  _ <*> (Val n) = Val n
  (Val n) <*> _ = Val n
  (Var f) <*> (Add l r) = fmap f (Add l r)
  (Add l r) <*> expr = Add l' r'
    where
      l' = l <*> expr
      r' = r <*> expr

instance Monad Expr where
  -- return :: a -> Expr a
  return = pure

  (Var x) >>= f = f x
  (Val n) >>= f = Val n
  (Add l r) >>= f = Add l' r'
    where
      l' = l >>= f
      r' = r >>= f

varx = Var ("x", Val 1)

vary = Var ("y", Val 1)

expr = Add varx vary

int2Expr :: Int -> Expr Int
int2Expr = Val

subs :: String -> Expr Int
subs "x" = Val 8
subs "y" = Val 7
subs _ = Val 0

substitude :: Expr String -> Expr Int
substitude expr = expr >>= subs

evaluate :: Expr String -> Int
evaluate = eval . substitude

eval :: Expr Int -> Int
eval (Val n) = n
eval (Add l r) = eval l + eval r

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  -- fmap (a -> b) -> ST a -> ST b
  fmap f stx = do
    x <- stx
    return (f x)

instance Applicative ST where
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> stx = do
    f <- stf
    x <- stx
    return (f x)

instance Monad ST where
  st >>= f =
    S
      ( \s ->
          let (a, s') = app st s
              (b, s'') = app (f a) s'
           in (b, s'')
      )
