import Data.List (tails)
import qualified Data.Map as Map
import qualified Data.Set as Set

data City = City { antennas :: Map.Map Char [Position], xSize :: Int, ySize :: Int }
     deriving (Show)

data Distance = Distance { xDist :: Int, yDist :: Int }
     deriving (Eq, Show)

class Movable a where
    move :: a -> Distance -> a

data Position = Position { xPos :: Int, yPos :: Int }
     deriving (Eq, Ord, Show)

instance Movable Position where
    move position distance =
        Position { xPos = xPos position + xDist distance, yPos = yPos position + yDist distance }

withinCity :: City -> Position -> Bool
withinCity city (Position x y) =
    x >= 0 && x < xSize city && y >= 0 && y < ySize city


getDistance :: Position -> Position -> Distance
getDistance (Position x1 y1) (Position x2 y2) =
    Distance { xDist = x2 - x1, yDist = y2 - y1 }

parseLine :: Map.Map Char [Position] -> (Int, String) -> Map.Map Char [Position]
parseLine inputPositions (lineNr, line) =
    foldl storePosition inputPositions $ zip [0..] line
    where
        storePosition positions (colNr, frequency)
            | frequency == '.' = positions
            | otherwise = Map.insertWith (++) frequency [Position colNr lineNr] positions

moveRecursive :: Position -> Distance -> [Position]
moveRecursive pos dist = pos : moveRecursive (move pos dist) dist

getAntinodeLine :: City -> Position -> Position -> [Position]
getAntinodeLine city p1 p2 =
    takeWhile (withinCity city) (moveRecursive topPos topDistance)
    ++ takeWhile (withinCity city) (moveRecursive bottomPos bottomDistance)
    where
        (topPos, bottomPos)
            | yPos p1 < yPos p2 = (p1, p2)
            | yPos p1 == yPos p2 && xPos p1 < xPos p2 = (p1, p2)
            | otherwise = (p2, p1)
        topDistance = getDistance bottomPos topPos
        bottomDistance = getDistance topPos bottomPos

findFrequencyAntinodes :: City -> [Position] -> [Position]
findFrequencyAntinodes city positions =
    concatMap (uncurry (getAntinodeLine city)) pairs
    where
        pairs = [(x,y) | (x:ys) <- tails positions, y <- ys]

findAntinodes :: City -> Set.Set Position
findAntinodes city =
    Set.fromList $ concatMap (filter (withinCity city) . findFrequencyAntinodes city) (antennas city)

main :: IO ()
main = do
    contents <- getContents
    let parsedLines = foldl parseLine Map.empty $ zip [0..] $ lines contents
    let yPositions = length $ lines contents
    let xPositions = length $ head $ lines contents
    let city = City { antennas = parsedLines, xSize = xPositions, ySize = yPositions }
    print $ length $ findAntinodes city
