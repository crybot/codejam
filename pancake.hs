

type Stack = [Char]

flipOne :: Char -> Char
flipOne '+' = '-'
flipOne '-' = '+'

--flipStack :: Int -> Stack -> Stack
--flipStack _ [] = []
--flipStack 0 xs = xs
--flipStack n (x:xs) = flipOne x : flipStack (n-1) xs
flipStack :: Stack -> Stack
flipStack = foldl (\xs x -> flipOne x : xs) []

solve :: Stack -> Int -> Int
solve [] c = c
solve stack@('-':xs) c = solve (flipStack left ++ right) (c+1)
    where (left,right) = span (=='-') stack
solve stack@('+':xs) c 
    | notElem '-' xs = c
    | otherwise = solve (flipStack left ++ right) (c+1)
    where (left,right) = span (=='+') stack

count :: Stack -> Int
count stack = solve stack 0

fuse :: Int -> Int -> String
fuse a b = "Case #" ++ (show a) ++ ": " ++ (show b)

main = do
    t <- getLine
    contents <- getContents
    putStr $ unlines $ zipWith fuse [1..] $ map count (lines contents)
