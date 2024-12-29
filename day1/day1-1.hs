import Data.List (sort, transpose)

readLocations :: [String] -> [Int]
readLocations [] = []
readLocations [_] = []
readLocations (a:b:_) = [read a, read b]

main :: IO ()
main = do
     contents <- getContents
     let rows = map (readLocations . words) $ lines contents
     let columns = map sort $ transpose rows
     let diffs = zipWith (\a b -> abs (a - b)) (head columns) (columns !! 1)
     print $ sum diffs
