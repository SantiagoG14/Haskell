data MessageType = Info | Warning | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String | Unknown String
  deriving (Show, Eq)

-- parseMessage :: String -> LogMessage

split :: Char -> String -> [String]
split _ [] = []
split sep (x : xs)
  | sep != x = [x] ++ split sep xs
  | otherwise = [] : split sep xs
