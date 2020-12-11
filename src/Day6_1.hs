import Data.List
import Data.Char

-- I cannot FUCKING believe I have to define a splitOn function. I guess hackage is down or something
-- Function works as expected.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim [] = [[]]
splitOn delim (c:cs)
  | c == delim = [] : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitOn delim cs

count :: Ord a => [a] -> Int
count = length . group . sort

-- the splitOn here will group thing by blank lines, since the input can be across multiple lines
solve :: String -> Int
solve = sum . map count . map trim . map unwords . splitOn "" . lines
  where trim = filter (not . isSpace)

main :: IO ()
main = interact $ show . solve
