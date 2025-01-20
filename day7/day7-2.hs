import Data.List.Split (splitOn)

data Equation = Equation { result :: Int, values :: [Int] }
    deriving (Show)

instance Read Equation where
    readsPrec _ input =
        [(Equation res vals, "")]
        where
            (res, vals) = getParts $ splitOn ":" input
            getParts [] = (0, [])
            getParts [r] = (read r, [])
            getParts (r:vs:_) = (read r, map read $ words vs)

combinations :: [a] -> Int -> [[a]]
combinations _ 0 = [[]]
combinations elements slots = [e:rest | e <- elements, rest <- combinations elements (slots - 1)]

concatNumbers :: Int -> Int -> Int
concatNumbers a b = read $ show a ++ show b

calculate :: [Int -> Int -> Int] -> [Int] -> Int
calculate _ [] = 0
calculate _ [v] = v
calculate [] _ = 0
calculate (f:fs) (v1:v2:vs) = calculate fs (f v1 v2 : vs)

hasMatchingOperators :: Equation -> Bool
hasMatchingOperators equation =
    any matches functions
    where
        functions = combinations [(+), (*), concatNumbers] $ length (values equation) - 1
        matches fns = calculate fns (values equation) == result equation

main :: IO ()
main = do
    contents <- getContents
    let equations = map read $ lines contents :: [Equation]
    let matching = map snd $ filter fst $ map (\e -> (hasMatchingOperators e, result e)) equations
    print $ sum matching
