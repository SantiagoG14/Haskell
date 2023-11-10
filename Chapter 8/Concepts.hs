type Pos = (Int, Int)

type Trans = Pos -> Pos

type Pair a = (a,a)

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data Move = North | South | East | West

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East (x, y) = (x + 1, y)
move West(x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float
    deriving(Eq, Ord)

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- recursive types

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0;
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
-- add m n = int2nat (nat2int m + nat2int n)
add Zero n = n
add (Succ m) n = Succ(add m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
    (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

btfOccurs :: Ord a => a -> Tree a -> Bool
btfOccurs x (Leaf y) = x == y
btfOccurs x (Node l y r) | x == y = True
                      | x < y = btfOccurs x l
                      | otherwise = btfOccurs x r

class MyEq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x == y)

instance MyEq Bool where 
    False == False = True
    True == True = True
    _ == _ = False

class MyEq a => Ord a where 
    (<), (<=), (>=), (>) :: a -> a -> Bool
    min, max :: a -> a -> a

    min x y | x <= y = x
            | otherwise = y
    
    max x y | x <= y = y
            | otherwise = x

instance MyOrd Bool where
    False < True = True
    _ < _ False

    b <= c = (b < c) || (b == c)
    b > c = c < b
    b >= c = c <= b
    
