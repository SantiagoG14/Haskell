-- 1
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = (x `mod` 10) : toDigits (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = zipWith (*) ns xs
  where
    ns = reverse (take l (cycle [1, 2]))
    l = length xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate x = x' == 0
  where
    x' = getValue x `mod` 10

getValue :: Integer -> Integer
getValue = sumDigits . doubleEveryOther . toDigits

-- 2
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n from to aux = hanoi (n - 1) from aux to ++ [(from, to)] ++ hanoi (n - 1) aux to from

