import Concepts (gametree, empty, Player(O,X,B), Grid, Tree(Node))

-- 1
tree = gametree [[O, B, B],[X, X, O],[X, O, B]] O
emptyTree = gametree empty O

verifyNodesGametree :: Int
verifyNodesGametree = (verify 0 tree)
    where tree = gametree empty O

verify :: Int -> Tree Grid -> Int
verify x (Node _ []) = 1 
verify x (Node _ ts) = (x + 1) + sum (map (verify 0) ts)

depths :: Int -> [Int] -> Tree Grid -> [Int]
depths x ds (Node _ []) = x:ds
depths x ds (Node _ ts) = concat (map (depths (x + 1) ds) ts)

maxdepth = maximum (depths 0 [] emptyTree)


main :: IO ()
main = do putStrLn (show maxdepth)
          putStr (show verifyNodesGametree)

