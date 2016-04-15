import Data.Bits
import Data.Digits
import Data.Char
import Numeric

type Coin = [Integer]

makeCoin n c = digits 2 $ truncate $ 2**(n-1) + 2*c
-- makeNaive n = 1 : replicate (n-2) 0 ++ [1]

factors x = [ y | y <- [1..x], y /= 1, y /= x, x `mod` y == 0] 

bases coin = map (\b -> unDigits b coin) [2..10]

divisors :: (Integral a) => [a] -> [[a]]
divisors = map (take 1 . factors)

valid = notElem [] . divisors . bases 

toString :: Coin -> String
toString = foldr ((:) . intToDigit . fromInteger) []

getCoins n j = map toString $ take j $ filter valid $ map (makeCoin n) [1..]

fuse :: Int -> Int -> String
fuse a b = "Case #" ++ (show a) ++ ": " ++ (show b)

main = do
    t <- getLine
    contents <- getContents
    putStrLn "Case #1: "
    putStr $ unlines $ getCoins 16 50
    --putStr $ unlines $ zipWith fuse [1..] $ map count (lines contents)
    return ()
