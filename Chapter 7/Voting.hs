import Data.List

votes :: [String]
votes = ["r", "b", "r", "b", "b", "r"]

ballots :: [[String]]
ballots = [["r", "g"], ["b"], ["g", "r", "b"], ["b", "g", "r"], ["g"]]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

unique :: Eq a => [a] -> [a] 
unique [] = []
unique (x:xs) = x : filter (/= x) (unique xs)

result :: Ord a => [a] -> [(Int, a)]
result vs = sort[(count v vs, v) | v <- unique vs]

winner :: Ord a => [a] -> a
winner = snd . last . result


rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter(/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
            [c] -> c
            (c:cs) -> winner' (elim c bs)