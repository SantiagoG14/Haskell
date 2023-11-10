import Data.Char
import System.IO

adder :: IO ()
adder = do putStr "How many numbers? "
           x <- getLine
           let n = read x :: Int
           xs <- sequence [getNumber | _ <- [1..n]]
           putStr "The total is "
           putStrLn (show (sum xs))

getNumber :: IO Int
getNumber = do x <- getLine
               return (read x :: Int)


getCh :: IO Char 
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = readLineAux ""

readLineAux :: String -> IO String
readLineAux cs = do c <- getCh
                    if c == '\n' then
                        -- do putStrLn ""
                           return cs
                    else if c == '\DEL' then
                        -- do putChar '\b'
                           readLineAux (take (length cs - 1) cs)
                    else 
                        -- do putStr [c]
                           readLineAux (cs ++ [c])

main :: IO ()
main = do str <- readLine
          putStrLn str