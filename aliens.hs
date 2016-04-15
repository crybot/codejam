import qualified Data.Set as Set
import Control.Monad

parse :: String -> [String]
parse "" = []
parse (x:xs)
    | x == '(' = (takeWhile f xs ) : (parse $ tail $ dropWhile f xs)
    | otherwise = [x] : (parse xs)
    where f = (/= ')')

main :: IO ()
main = do
    l:d:n:[] <- map read . words <$> getLine :: IO [Int]
    dictionary <- Set.fromList <$> replicateM d getLine
    forM_ [1..n] $ \x -> do
        putStrLn $ "Case #" ++ show x ++ ": "
        return ()



