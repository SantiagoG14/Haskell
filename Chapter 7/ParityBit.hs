import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum[w * b | (w,b) <- zip weight bits]
               where weight = iterate (*2) 1 


bin2int2 :: [Bit] -> Int
bin2int2 = foldr(\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin(n `div` 2)


make9 :: [Bit] -> [Bit]
make9 bits = parityBit bits : take 8 (bits ++ repeat 0)


parityBit :: [Bit] -> Bit
parityBit = (`mod` 2) . length . filter(== 1)


encode :: String -> [Bit]
encode = concat .  map(make9 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

decode :: [Bit] -> String
decode = map(chr  . bin2int . checkParityBit) . chop9

checkParityBit :: [Bit] -> [Bit]
checkParityBit (bit: bits) | parityBit (bits) == bit = bits
                           | otherwise = error "Wrong parity bit found"



transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail