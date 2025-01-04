import Data.Array (Array, bounds, indices, listArray, (!))

data Position = Position { xPos :: Int, yPos :: Int }
     deriving (Show)

data Direction = Direction { xDir :: Int, yDir :: Int }
     deriving (Show)

up :: Direction
up = Direction { xDir = 0, yDir = -1 }
upright :: Direction
upright = Direction { xDir = 1, yDir = -1 }
right :: Direction
right = Direction { xDir = 1, yDir = 0 }
downright :: Direction
downright = Direction { xDir = 1, yDir = 1 }
down :: Direction
down = Direction { xDir = 0, yDir = 1 }
downleft :: Direction
downleft = Direction { xDir = -1, yDir = 1 }
left :: Direction
left = Direction { xDir = -1, yDir = 0 }
upleft :: Direction
upleft = Direction { xDir = -1, yDir = -1 }

allDirections :: [Direction]
allDirections = [up, upright, right, downright, down, downleft, left, upleft]

move :: Position -> Direction -> Position
move position direction =
     Position { xPos = xPos position + xDir direction, yPos = yPos position + yDir direction }

withinBounds :: Array Int (Array Int Char) -> Position -> Bool
withinBounds rows (Position x y) =
      y >= minboundY && y <= maxboundY && x >= minboundX && x <= maxboundX
      where
          (minboundY, maxboundY) = bounds rows
          (minboundX, maxboundX) = bounds (rows ! y)

traceWord :: Array Int (Array Int Char) -> Direction -> String -> Position -> Bool
traceWord _ _ [] _ = True
traceWord rows direction (char:rest) position =
     withinBounds rows position &&
         currentChar == char &&
         traceWord rows direction rest newPosition
     where
          currentChar = rows ! yPos position ! xPos position
          newPosition = move position direction

countXmas :: Array Int (Array Int Char) -> Position -> Int
countXmas rows position =
     if currentChar == 'X' then
          length $ filter id $ map traceDirection allDirections
     else
          0
     where
          currentChar = rows ! yPos position ! xPos position
          traceDirection direction = traceWord rows direction "XMAS" position

getTotalXmasCounts :: Array Int (Array Int Char) -> [[Int]]
getTotalXmasCounts rows =
     map countCol $ indices rows
     where
          countCol y = map (countRow y) (indices (rows ! y))
          countRow y x = countXmas rows (Position x y)

main :: IO ()
main = do
     contents <- getContents
     let arrays = map (\line -> listArray (0, length line - 1) line) $ lines contents
     let inData = listArray (0, length arrays - 1) arrays
     let xmases = getTotalXmasCounts inData
     print $ sum $ map sum xmases
