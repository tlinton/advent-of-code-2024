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

-- isXmas checks for four cases. It always starts at the top left corner of the
-- word.
--
-- M M   M S   S M   S S
--  A     A     A     A
-- S S   M S   S M   M M
isXmas :: Array Int (Array Int Char) -> Position -> Bool
isXmas rows position =
     any findOneCase [("MAS", "MAS"), ("MAS", "SAM"), ("SAM", "MAS"), ("SAM", "SAM")]
     where
          findOneCase (word1, word2) = traceWord rows downright word1 position &&
               traceWord rows downleft word2 nextPosition
          nextPosition = move (move position right) right

getTotalXmases :: Array Int (Array Int Char) -> [[Bool]]
getTotalXmases rows =
     map countCol $ indices rows
     where
          countCol y = map (countRow y) (indices (rows ! y))
          countRow y x = isXmas rows (Position x y)

main :: IO ()
main = do
     contents <- getContents
     let arrays = map (\line -> listArray (0, length line - 1) line) $ lines contents
     let inData = listArray (0, length arrays - 1) arrays
     let xmases = getTotalXmases inData
     print $ sum $ map (length . filter id) xmases
