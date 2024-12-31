checkLevels :: (Ord a, Num a) => a -> a -> [a] -> Bool
checkLevels _ _ [] = False
checkLevels _ _ [_] = False
checkLevels minDiff maxDiff (x1:x2:xs)
     | x1 - x2 < minDiff = False
     | x1 - x2 > maxDiff = False
     | null xs = True
     | otherwise = checkLevels minDiff maxDiff (x2:xs)

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe [] = False
isSafe [_] = False
isSafe (a:b:rest)
     | a > b = checkLevels 1 3 (a:b:rest)
     | a < b = checkLevels (-3) (-1) (a:b:rest)
     | otherwise = False

getPermutations :: [a] -> [[a]]
getPermutations [] = []
getPermutations [x] = [[x]]
getPermutations (x:xs) = (x:xs) : drop1 [] x xs
     where
          drop1 left _ [] = [left]
          drop1 left current (right:rs) =
               (left ++ (right:rs)) : drop1 (left ++ [current]) right rs

main :: IO ()
main = do
     contents <- getContents
     let reports = map (map read . words) $ lines contents :: [[Int]]
     print $ length $ filter id $ map (any isSafe . getPermutations) reports
