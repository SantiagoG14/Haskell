import Data.Char
import Data.List

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial  = [7,6,5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
        where update r n | r == row && valid board row num  = n - num
                         | otherwise = n

                        

-- IO utilities 

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))


putBoard :: Board -> IO ()
-- putBoard = putBoardAux 1
putBoard xs = sequence_ [putRow n x | (n,x) <- zip[1..n] xs]
            where n = length xs
-- putBoard [a,b,c,d,e] = do putRow 1 a
--                           putRow 2 b
--                           putRow 3 c
--                           putRow 4 d
--                           putRow 5 e

putBoardAux :: Int -> Board -> IO ()
putBoardAux _ [] = return ()
putBoardAux n (x:xs)= do putRow n x
                         putBoardAux (n + 1) xs
-- putBoardAux n xs = _sequence[putRow n x | (n,x) <- zip [1..n] xs]




getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getLine

                     let x1 = if length x == 1 then x !! 0 else '-'
                     if isDigit x1 then
                        return (digitToInt x1)
                     else 
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt 

newLine :: IO ()
newLine = putChar '\n'

game :: Board -> Int -> IO ()
game board player = do newLine
                       putBoard board
                       if finished board then
                          do newLine
                             putStr "Player "
                             putStr (show (next player))
                             putStrLn " wins!!"
                       else
                          do newLine
                             putStr "Player "
                             putStrLn (show player)
                             row <- getDigit "Enter a row number: "
                             num <- getDigit "Stars to remove : "
                             if valid board row num then
                                game (move board row num) (next player)
                             else 
                                do newLine
                                   putStrLn "ERROR: Invalid move"
                                   game board player 

play :: IO ()
play = game initial 1
