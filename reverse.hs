import Control.Monad

reverseLine :: String -> String
reverseLine = unwords . reverse . words 

main :: IO ()
main = do
    n <- readLn
    forM_ [1..n] $ \i -> do
        line <- getLine
        putStrLn $ "Case #" ++ show i ++ ": " ++ reverseLine line
