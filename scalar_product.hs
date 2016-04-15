import Control.Monad
import Data.List

scalarP :: [Int] -> [Int] -> Int
scalarP a b = sum $ zipWith (*) a b

main :: IO ()
main = do
    t <- readLn
    forM_ [1..t] $ \k -> do
        getLine -- reads n
        v1 <- map read . words <$> getLine
        v2 <- map read . words <$> getLine
        let (v1',v2') = (sort v1, sortBy (flip compare) v2) 
        putStrLn $ "Case #" ++ show k ++ ": " ++ show (scalarP v1' v2')


