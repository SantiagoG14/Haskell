import Distribution.Simple.Utils (xargs)
-- 1
factorial 0 = 1
factorial n | n >= 0 = n * factorial (n-1)
            | otherwise = 0

-- 2

sumdown :: Int -> Int 
sumdown 0 = 0
sumdown n | n >= 0 = n + sumdown (n-1)
          | otherwise = 0


-- 3
(^^) :: Int -> Int -> Int

_ ^^ 0 = 1
x ^^ y | y > -1 = x * (x Main.^^ (y-1))

-- 4

euclid :: Int -> Int -> Int 
euclid x y | x == y = x
           | x < y = euclid x (y-x)
           | otherwise = euclid y (x-y)


-- 6.a

myAnd :: [Bool] -> Bool 
myAnd [] = True
myAnd (False : _) = False 
myAnd (True : xs) = myAnd xs

-- 6.b 

myConcat :: [[a]] -> [a]
myConcat (x : []) = x
myConcat (x : xs) = x ++ myConcat xs


--6.c

myRep :: Int -> a -> [a]
myRep 0 _ = []
myRep x a = [a] ++ myRep (x-1) a

--6.d

(!!!) :: [a] -> Int -> a
(x:_) !!! 0 = x
(x:xs) !!! n  =  xs !!! (n-1)


myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs) = x == a  || myElem a xs


-- 7 

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = [] 
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = [x] ++ merge xs (y:ys)
    | otherwise = [y] ++ merge (x:xs) ys


msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort l) (msort r)
    where (l,r) = halve xs


halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve xs = (take l xs, drop l xs)
    where l = (length xs) `div` 2

