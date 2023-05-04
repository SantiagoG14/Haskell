-- 1

applFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
applFilter f p =  map f . filter p


-- 2.a

all2 :: (a -> Bool) -> [a] -> Bool
all2 f = and . map f 

-- 2.b

any2 :: (a -> Bool) -> [a] -> Bool
any2 f = or . map f


-- 2.c

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f [] = []
takeWhile2 f (x:xs) | f x = x : takeWhile f xs
                    | otherwise = []

-- 2.c

dropWhile2 :: (a -> Bool) -> [a] -> [a]
dropWhile2 f [] = []
dropWhile2 f (x:xs) | f x = dropWhile2 f xs
                    | otherwise = x:xs

-- 3.a

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []

-- 3.b

applyP :: (a -> Bool) -> a -> [a]
applyP p x | p x = []
           | otherwise = [x]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x y -> (applyP p x) ++ y) []


-- 4
zipRevIndex :: [a] -> [(a, Int)]
zipRevIndex xs = zip xs (reverse [0..l])
                where l = length xs - 1

dec2int :: [Int] -> Int
dec2int = foldl addToPower 0 . zipRevIndex

addToPower :: Int -> (Int, Int) -> Int
addToPower a (b,i) = a + (b*(10^i))


-- 5

myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)


myUnCurry :: (a -> b -> c) -> (a, b) -> c
myUnCurry f (x, y) = f x y

myAdd :: Int -> Int -> Int
myAdd x y = x + y


-- 6


type Bit = Int

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

newInt2bin = unfold (== 0) (`mod` 2) (`div` 2)


newChop8 :: [Bit] -> [[Bit]]
newChop8 = unfold (== []) (take 8) (drop 8)

unfoldMap :: (a -> b) -> [a] -> [b]
unfoldMap f = unfold (null) (f . head) tail


unfoldIter :: (a -> a) -> a -> [a]
unfoldIter f = unfold (\x -> False) (id) (f)


-- 8

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g []       = []
altMap f g (x : xs) = f x : altMap g f xs










