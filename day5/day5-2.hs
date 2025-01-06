import Data.List (sortBy)
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

orderKey :: Order -> Int -> Int -> Ordering
orderKey orders a b
  | a `comesBefore` b = LT
  | b `comesBefore` a = GT
  | otherwise = EQ
  where
      x `comesBefore` y = case Map.lookup x orders of
            Just ys -> y `elem` ys
            Nothing -> False

main :: IO ()
main = do
     contents <- getContents
     let (orders, pageLines) = parseOrders Map.empty $ lines contents
     let pages = parsePages pageLines
     let faulty = filter (not . fst) $ zip (map (isInOrder orders) pages) pages
     let corrected = map (sortBy (orderKey orders) . snd) faulty
     let middleCorrected = map (\list -> list !! (length list `div` 2)) corrected
     print $ sum middleCorrected
