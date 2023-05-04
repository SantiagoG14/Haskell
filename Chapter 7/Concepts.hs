twice :: (a -> a) -> a -> a
twice f x = f (f x)

map1 :: (a -> b) -> [a] -> [b]
map1 f xs = [f x | x <- xs]

map2 :: (a -> b) -> [a] -> [b]
map2 f [] = []
map2 f (x:xs) = f x : map f xs


suma :: Int -> Int -> Int
suma x y = x + y

suma1 :: Int -> Int
suma1 x = suma 1 x

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]


reverse1 :: [a] -> [a]
reverse1 = foldr snoc []


sumsqeven = sum . map (^2) . filter even


compose :: [a -> a] -> (a -> a)
compose  = foldr (.) id
