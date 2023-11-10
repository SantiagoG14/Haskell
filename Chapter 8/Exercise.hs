import Data.List
-- #1

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0;
nat2int (Succ n) = 1 + nat2int n


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

two = int2nat 2
three = int2nat 3

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ(add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n =  add n (mult m n)


-- #2

data Tree a = Leaf a | Node (Tree a) a (Tree a)

btfOccurs :: Ord a => a -> Tree a -> Bool
btfOccurs x (Leaf y) = x == y
btfOccurs x (Node l y r) | x == y = True
                      | x < y = btfOccurs x l
                      | otherwise = btfOccurs x r



t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
    (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of 
                        LT -> occurs x l
                        EQ -> True
                        GT -> occurs x r

-- #3

data Tree2 a  = Leaf2 a | Node2 (Tree2 a) (Tree2 a)
    deriving Show

t2 :: Tree2 Int
t2 = Node2 (Node2 (Leaf2 1) (Leaf2 4)) 
    (Node2 (Leaf2 6) (Leaf2 9))

t3 :: Tree2 Int
t3 = Node2 (Node2 (Leaf2 1) (Node2 (Leaf2 3) (Leaf2 5))) (Leaf2 4)

getLeaves :: Tree2 a -> Int
getLeaves (Leaf2 x) = 1
getLeaves (Node2 l r) = getLeaves l + getLeaves r

balanced :: Tree2 a -> Bool 
balanced (Node2 l r) = abs (getLeaves l - getLeaves r) <= 1

-- #4

splitArr :: [a] -> ([a], [a])
splitArr x = (take half x, drop half x)
           where half = (length x) `div` 2

balance :: [a] -> Tree2 a
balance [] = error "list is empty"
balance [x] = Leaf2 x
balance [l, r] = Node2(Leaf2 l)(Leaf2 r)
balance x = Node2(balance l) (balance r) 
            where (l,r) = splitArr x

-- #5

data Expr = Val Int | Add Expr Expr

ex = Add (Add (Val 2) (Val 4)) (Val 3)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- #6

eval :: Expr -> Int
eval e = folde (\x -> x) (\x y -> x + y) e

size :: Expr -> Int
size e =  folde (\x -> 1) (\x y -> x + y) e

-- #7


data MyMaybe a = MyJust a | MyNothing

-- instance Eq a => Eq (MyMaybe a) where
--     MyJust x == MyJust y = x == y
--     _ == _ = False

-- -- newtype MyArr a = MyArr [a]
-- instance Eq a => Eq [a] where
--     [] == [] = True
--     (x:xs) == (y:ys) = x == y && xs == ys
    
-- #8
--            ()   {}   []


isValid :: String -> Bool
isValid ('(' : xs) = isValidOpen xs ['(']
isValid ('{' : xs) = isValidOpen xs ['{']
isValid ('[' : xs) = isValidOpen xs ['[']
isValid _ = False

isValidOpen :: String -> [Char] -> Bool
isValidOpen ('(':xs) bs = isValidOpen xs ('(':bs)
isValidOpen (')' : []) ('(' : []) = True
isValidOpen (')' : xs) ('(' : bs) = isValidOpen xs bs

isValidOpen ('{':xs) bs = isValidOpen xs ('{':bs)
isValidOpen ('}' : []) ('{' : []) = True
isValidOpen ('}' : xs) ('{' : bs) = isValidOpen xs bs

isValidOpen ('[':xs) bs = isValidOpen xs ('[':bs)
isValidOpen (']' : []) ('[' : []) = True
isValidOpen (']' : xs) ('[' : bs) = isValidOpen xs bs

isValidOpen _ _ = False


trap :: [Int] -> Int
trap [] = 0
trap [h] = 0
trap hs = sumAllLayer 1 hs 0

-- trap hs = sum(map (\l -> sumTrapLayer l hs) iter)
--         where 
--             iter = [1..m]
--             m = maximum hs

sumAllLayer :: Int -> [Int] -> Int -> Int
sumAllLayer l hs s | sumLayer == 0 && s > 0  = s
                   | otherwise = sumAllLayer (l +1) hs (sumLayer + s)
            where sumLayer = sumTrapLayer l hs

sumTrapLayer :: Int -> [Int] -> Int
sumTrapLayer l = sum . truncateLayerLeft . truncateLayerRight . trapLayer l

trapLayer :: Int -> [Int] -> [Int]
trapLayer l xs = map (\x -> if x - l >= 0 then 0 else 1) xs

truncateLayerLeft :: [Int] -> [Int]
truncateLayerLeft (0:xs) = xs
truncateLayerLeft (1:xs) = truncateLayerLeft xs
truncateLayerLeft _ = []

truncateLayerRight :: [Int] -> [Int]
truncateLayerRight [] = []
truncateLayerRight xs | last xs == 1 = truncateLayerRight (take (length xs - 1) xs)
                      | otherwise =  xs

trap' :: [Int] -> Int
trap' hs = trapTwoPointers 0 2 0 hs

trapTwoPointers :: Int -> Int -> Int -> [Int] -> Int
trapTwoPointers l r s hs | r > length hs - 1 && l >= length hs - 2  = s
                         | r == length hs && r - l > 2  = trapTwoPointers nb (nb + 3) ((trapWater l nb pl (hs!!nb) hs) + s) hs -- find the nearest biggest height and get the trapped water 
                         | pl > pr                      = trapTwoPointers l (r + 1) s hs -- move r to the right when left value is greater than right
                         | pl > 0 && pr > 0 && pl <= pr = trapTwoPointers r (r + 2) (s + (trapWater l r pl pr hs)) hs -- add trapped water to sum, 
                         | otherwise                    = trapTwoPointers (l + 1) (r + 1) s hs
                         where 
                            pl = hs!!l
                            pr = hs!!r
                            m = min pl pr
                            nb = nearestBiggest l hs -- nearest biggest

trapWater :: Int -> Int -> Int -> Int -> [Int] -> Int
trapWater l r pl pr hs = area - blocksArea
                     where
                        width = r - l + 1
                        height = min pl pr
                        area = width * height
                        blocks = slice l r hs
                        blocksArea = (sum blocks) - abs (pr - pl)


nearestBiggest :: Int -> [Int] -> Int
nearestBiggest l hs = case elemIndex m hs' of
                            Nothing -> l + 2
                            Just 0 -> nearestBiggest (l + 1) hs
                            Just n -> n + l + 1
                      where 
                        hs' = drop (l + 1) hs
                        m = maximum hs'

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)
