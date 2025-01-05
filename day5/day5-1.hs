import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map

type Order = Map.Map Int [Int]

parsePages :: [String] -> [[Int]]
parsePages = map (map read . splitOn ",")

parseOrders :: Order -> [String] -> (Order, [String])
parseOrders order [] = (order, [])
parseOrders order (line:rest) =
     case splitOn "|" line of
          [a, b] -> parseOrders (Map.insertWith (++) (read a) [read b] order) rest
          _ -> (order, rest)

pageInOrder :: Order -> [Int] -> Int -> [Int] -> Bool
pageInOrder orders left x right
     | null right = not $ any (x `comesBefore`) left
     | otherwise =
          if any (x `comesBefore`) left then
               False
          else
               checkNext right
     where
          checkNext [] = True
          checkNext (r:rs) = pageInOrder orders (left ++ [x]) r rs
          a `comesBefore` b = case Map.lookup a orders of
               Just bs -> b `elem` bs
               Nothing -> False

isInOrder :: Order -> [Int] -> Bool
isInOrder _ [] = True
isInOrder order (r:rs) = pageInOrder order [] r rs

main :: IO ()
main = do
     contents <- getContents
     let (orders, pageLines) = parseOrders Map.empty $ lines contents
     let pages = parsePages pageLines
     let matching = filter fst $ zip (map (isInOrder orders) pages) pages
     let middleMatching = map ((\list -> list !! (length list `div` 2)) . snd) matching
     print $ sum middleMatching
