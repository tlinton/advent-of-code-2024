{-# LANGUAGE InstanceSigs #-}
import Data.Array (Array, bounds, listArray, (!))
import qualified Data.Set as Set

data Square = Empty | Obstacle
    deriving (Eq, Show)

type Room = Array Int (Array Int Square)

class Turnable a where
    turnRight :: a -> a

data Direction = Direction { xDir :: Int, yDir :: Int }
     deriving (Eq, Show)

instance Turnable Direction where
    turnRight :: Direction -> Direction
    turnRight direction
        | direction == up = right
        | direction == right = down
        | direction == down = left
        | direction == left = up
        | otherwise = direction

class Movable a where
    move :: a -> Direction -> a

data Position = Position { xPos :: Int, yPos :: Int }
     deriving (Eq, Ord, Show)

instance Movable Position where
    move position direction =
        Position { xPos = xPos position + xDir direction, yPos = yPos position + yDir direction }

data Guard = Guard { guardPos :: Position, guardDir :: Direction }
     deriving (Show)

instance Movable Guard where
    move :: Guard -> Direction -> Guard
    move guard direction =
        Guard newPos $ guardDir guard
        where newPos = move (guardPos guard) direction

instance Turnable Guard where
    turnRight :: Guard -> Guard
    turnRight guard = Guard (guardPos guard) $ turnRight $ guardDir guard

squareType :: Room -> Position -> Square
squareType room (Position x y) = room ! y ! x

moveGuard :: Room -> Guard -> Guard
moveGuard room guard
    -- It is allowed for the guard to move out from the room, so if the
    -- position is not within the room, the guard will continue moving without
    -- any checks.
    | not (insideRoom room (guardPos next)) = next
    | squareType room (guardPos next) == Obstacle = moveGuard room $ turnRight guard
    | otherwise = next
    where
        next = move guard $ guardDir guard

walk :: Room -> Guard -> [Position]
walk room guard =
    guardPos guard : walk room (moveGuard room guard)

up :: Direction
up = Direction { xDir = 0, yDir = -1 }
right :: Direction
right = Direction { xDir = 1, yDir = 0 }
down :: Direction
down = Direction { xDir = 0, yDir = 1 }
left :: Direction
left = Direction { xDir = -1, yDir = 0 }

insideRoom :: Room -> Position -> Bool
insideRoom room (Position x y) =
      y >= minboundY && y <= maxboundY && x >= minboundX && x <= maxboundX
      where
          (minboundY, maxboundY) = bounds room
          (minboundX, maxboundX) = bounds (room ! y)

createRoom :: [String] -> Room
createRoom rows =
    listArray (0, length arrays - 1) arrays
    where
        arrays = map ((\line -> listArray (0, length line - 1) line) . map createSquare) rows
        createSquare char = if char == '#' then Obstacle else Empty

createGuard :: [String] -> Guard
createGuard rows =
    Guard { guardPos = Position { xPos = x, yPos = y }, guardDir = up }
    where
        -- This will error if there is no guard, which is an allowed shortcut
        -- when doing advent of code.
        (x, y) = head [(x1, y1) | (y1, row) <- zip [0..] rows, (x1, char) <- zip [0..] row, char == '^']

main :: IO ()
main = do
    contents <- getContents
    let room = createRoom $ lines contents
    let guard = createGuard $ lines contents
    let visitedPositions = Set.fromList $ takeWhile (insideRoom room) $ walk room guard
    print $ Set.size visitedPositions
