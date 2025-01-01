import Data.Text (Text, pack, unpack)
import qualified Text.Regex.Pcre2 as Regex
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)) )

parseMultiplications :: [NonEmpty Text] -> Bool -> [(Int, Int)]
parseMultiplications [] _ = []
parseMultiplications (current:rest) active =
     case current of
          -- operation :| [x, y] -> (read $ unpack x, read $ unpack y)
          op :| [x, y] -> case unpack op of
                "do()" -> parseMultiplications rest True
                "don't()" -> parseMultiplications rest False
                -- The regexp only matches "mul", "do" and "don't", so this
                -- is always a "mul".
                _ -> if active then
                        (read $ unpack x, read $ unpack y) : parseMultiplications rest active
                     else
                        parseMultiplications rest active
          _ -> parseMultiplications rest active

main :: IO ()
main = do
     contents <- fmap pack getContents
     let regex = pack "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"
     let multiplications = Regex.captures regex contents
     --mapM_ (mapM_ (print . unpack)) multiplications
     let factors = parseMultiplications multiplications True
     print $ sum $ map (uncurry (*)) factors
     putStrLn "Done!"
