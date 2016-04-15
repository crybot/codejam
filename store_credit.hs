import Data.List.Split
import Control.Monad
import Data.List

combos :: [Int] -> [(Int, Int)]
combos list = [(head x, y) | x <- init $ tails list, y <- tail x]


process :: Int -> [Int] -> (Int,Int)
process c list =
    (minimum $ elemIndices a' list, maximum $ elemIndices b' list)
    where Just (a',b') = find (\(x,y) -> x + y == c) $ combos list


go :: IO ()
go = do
    c <- readLn :: IO Int
    i <- readLn :: IO Int
    items <- fmap words getLine
    let (a, b) = process c (map read items)
    putStrLn $ show (a+1) ++ " " ++ show (b+1)


main :: IO ()
main = do
    n <- readLn :: IO Int
    forM_ [1..n] $ \i -> do
        putStr $ "Case #" ++ show i ++ ": "
        go
