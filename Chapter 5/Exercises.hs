--1. Using a list comprehesions give an expresion that calculates the sum 1^2 + 2^2+ ... + 100^2 of the first 100 integerSquares

integerSquares :: Int
integerSquares = sum [x^2 | x <- [1..100]]

--2. Grid

grid :: Int -> Int -> [(Int, Int)]
grid n m = [(i,j) | i <-[0..n], j <- [0..m] ]

--3 square

square :: Int -> [(Int, Int)]

square n = [(x, y) | (x , y) <- grid n n, x /= y]

--4
myReplicate :: Int -> a -> [a]
myReplicate n x = [x  | _ <-  [1..n]]

--5 pyths 
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y , z) | x <- [1..n], y <- [1..n], z <- [1..n], x ^ 2 + y ^ 2 == z ^ 2]

--6 Perfect number
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect xs = head xsr == sum (tail xsr)
    where
        xsr = reverse xs

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect (factors x) ]

--7 [(x,y) | x <- [1,2], y <- [3,4]]


--8 

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

find k t = [v | (k', v) <- t, k == k']
newPositions x xs = find  x (zip xs [0..])
--9
scalarProduct xs ys = sum [ x * y | (x, y) <- zip xs ys ]
