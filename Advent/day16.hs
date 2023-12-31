data Direction = LeftDir | RightDir | Down | Up  
data Tile = Empty | MirrorFor | MirrorBack | SplitterVert | SplitterHori
                deriving (Eq, Ord, Show)
type Pos = (Int, Int)
-- followBeam :: Direction -> [[Char]] -> [[Char]]

getNextDirection :: Direction -> Tile -> [Direction]
getNextDirection a Empty = [a]
getNextDirection RightDir MirrorFor = [Up] -- > / ^
getNextDirection RightDir MirrorBack = [Down] -- > / V
getNextDirection RightDir SplitterVert = [Down, Up] -- > | V ^
getNextDirection RightDir SplitterHori = [RightDir] -- > - >
getNextDirection LeftDir MirrorFor = [Down] --v / <
getNextDirection LeftDir MirrorBack = [Up] -- ^ / <
getNextDirection LeftDir SplitterVert = [Down, Up]--v ^ | <
getNextDirection LeftDir SplitterHori = [LeftDir]-- < - <
getNextDirection Down MirrorFor = [LeftDir] 
getNextDirection Down MirrorBack = [RightDir] 
getNextDirection Down SplitterVert= [Down] 
getNextDirection Down SplitterHori= [LeftDir, RightDir] 
getNextDirection Up MirrorFor = [RightDir] 
getNextDirection Up MirrorBack = [LeftDir] 
getNextDirection Up SplitterVert= [Up] 
getNextDirection Up SplitterHori= [LeftDir,RightDir] 


charToTile :: Char -> Tile
charToTile '.' = Empty
charToTile '/' = MirrorFor 
charToTile '\\' = MirrorBack 
charToTile '|' = SplitterVert 
charToTile '-' = SplitterHori

move :: Pos -> Direction -> Pos
move (x,y) LeftDir =  (x + 1, y)
move (x,y) RightDir =  (x + 1, y)



