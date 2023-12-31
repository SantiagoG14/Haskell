data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

instance Functor Tree where 
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap g (Leaf a)   = Leaf (g a)
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

instance Applicative [] where
    -- pure :: a -> [a]
    pure x = [x]

    --(<*>) :: [(a-> b)] -> [a] -> [b]
    [] (<*>) xs  = [xs]
    gs (<*>) xs = [g x | g <- gs, x <- xs]

inc :: Functor f => f Int -> f Int
inc = fmap (+1)

fmap2 :: (a -> b -> c) -> f a -> f b -> f c
fmap2 g x y = pure g <*> x <*> y

