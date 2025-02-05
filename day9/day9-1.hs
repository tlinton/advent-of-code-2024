import Data.Char (isDigit)

data Block = File Int | Empty
    deriving (Eq, Show)

parseEmptyBlock :: Int -> String -> [Block]
parseEmptyBlock _ [] = []
parseEmptyBlock fileId (blockLength:rest) =
    if isDigit blockLength then
        replicate (read [blockLength]) Empty ++ parseFileBlock fileId rest
    else
        []

parseFileBlock :: Int -> String -> [Block]
parseFileBlock _ [] = []
parseFileBlock fileId (blockLength:rest) =
    if isDigit blockLength then
        replicate (read [blockLength]) (File fileId) ++ parseEmptyBlock (fileId + 1) rest
    else
        []

compactPart :: Int -> [Block] -> [Block] -> [Block]
compactPart _ [] _ = []
compactPart _ diskMap [] = diskMap
compactPart moves (diskMap:restMap) (toMove:restMove)
    | moves == 0 = []
    | diskMap == Empty = toMove : compactPart (moves - 1) restMap restMove
    | otherwise = diskMap : compactPart (moves - 1) restMap (toMove:restMove)

compact :: [Block] -> [Block]
compact [] = []
compact diskMap =
    compactPart numberOfMoves diskMap blocksToMove
    where
        blocksToMove = reverse (filter (/= Empty) diskMap)
        numberOfMoves = length blocksToMove

calculateChecksum :: Int -> [Block] -> Int
calculateChecksum _ [] = 0
calculateChecksum blockPos (diskMap:restMap) =
    case diskMap of
        File fileId -> blockPos * fileId + calculateChecksum (blockPos + 1) restMap
        _ -> calculateChecksum (blockPos + 1) restMap

main :: IO ()
main = do
    diskMap <- fmap (parseFileBlock 0) getContents
    let compacted = compact diskMap
    let checksum = calculateChecksum 0 compacted
    print checksum
