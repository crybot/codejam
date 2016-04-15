import qualified Data.Set as Set
import Data.Maybe

makeSet :: Int -> Set.Set Char
makeSet = Set.fromList . show

count :: Int -> Int -> Set.Set Char -> Maybe Int
count 0 _ _ = Nothing
count n m digits 
    | digits == (Set.fromList ['0'..'9']) = Just n
    | otherwise = count (n+m) m $ digits `Set.union` (makeSet (n+m))

sheep :: Int -> String
sheep 0 = "INSOMNIA"
sheep n = show $ fromJust $ count n n (makeSet n)

fuse :: Int -> String -> String
fuse a b = "case #" ++ (show a) ++ ": " ++ b
main = do
    t <- getLine
    contents <- getContents
    putStr $ unlines $ zipWith fuse [1..] $ map (sheep . read) (lines contents)


