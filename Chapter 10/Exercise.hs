import Data.Char

adder :: IO ()
adder = do putStr "How many numbers? "
           x <- getLine
           let n = read x :: Int
           adderAux n []
           
adderAux :: Int -> [Int] -> IO ()
adderAux 0 xs = do putStr "The total is " 
                   putStrLn (show (sum xs))
adderAux n xs = do c <- getLine
                   let x = read c :: Int
                   adderAux (n - 1) (x:xs)
                

