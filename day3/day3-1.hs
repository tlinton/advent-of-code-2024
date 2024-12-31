import Data.Text (Text, pack, unpack)
import qualified Text.Regex.Pcre2 as Regex
import Data.List.NonEmpty (NonEmpty, NonEmpty((:|)) )

parseMultiplications :: NonEmpty Text -> (Int, Int)
parseMultiplications input =
     case input of
          _ :| [x, y] -> (read $ unpack x, read $ unpack y)
          _ -> (0, 0)

main :: IO ()
main = do
     contents <- fmap pack getContents
     let regex = pack "mul\\(([0-9]+),([0-9]+)\\)"
     let multiplications = Regex.captures regex contents
     let factors = map parseMultiplications multiplications
     print $ sum $ map (uncurry (*)) factors
     putStrLn "Done!"
