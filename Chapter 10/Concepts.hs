import System.IO

myAct :: IO (Char, Char)
myAct = do x <- getChar
           getChar 
           y <- getChar
           return (x, y)

getMyLine :: IO String
getMyLine = do x <- getChar
               if x == '\n' then
                    return []
                else 
                    do xs <- getMyLine
                       return (x:xs)

putMyStr :: String -> IO ()
putMyStr [] = return ()
putMyStr (x:xs) = do putChar x
                     putMyStr xs

putMyStrLn :: String -> IO ()
putMyStrLn xs = do putMyStr xs
                   putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getMyLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine 
               if guess == word then
                  putStrLn "You got it!!"
                else 
                    do putStrLn (match word guess)
                       play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]


sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
              else 
                do putChar '_'
                   xs <- sgetLine
                   return (x:xs)



getCh :: IO Char 
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x