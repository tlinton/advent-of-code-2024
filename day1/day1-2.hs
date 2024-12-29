import Data.List (transpose)
import qualified Data.Map.Strict as Map

readLocations :: [String] -> [Int]
readLocations [] = []
readLocations [_] = []
readLocations (a:b:_) = [read a, read b]

main :: IO ()
main = do
     contents <- getContents
     let rows = map (readLocations . words) $ lines contents
     let transposed = transpose rows
     let leftList = head transposed
     let rightList = transposed !! 1
     let rightOccurrences = Map.fromListWith (+) $ zip rightList (repeat 1)
     let leftOccurrences = map (\x -> x * Map.findWithDefault 0 x rightOccurrences) leftList
     print $ sum leftOccurrences
