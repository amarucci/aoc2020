-- I cannot FUCKING believe I have to define a splitOn function. I guess hackage is down or something
-- Function works as expected.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delim [] = [[]]
splitOn delim (c:cs)
  | c == delim = [] : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = splitOn delim cs

isValid :: String -> Bool
isValid = (== 7) . length . filter id . map validField . filter (not . isCid) . map (tuplify . (splitOn ':')) . words
  where tuplify [x, y] = (x, y)
        isCid (x, _) = x == "cid"
        validField (x, _)
          | x == "byr" = True
          | x == "iyr" = True
          | x == "eyr" = True
          | x == "hgt" = True
          | x == "hcl" = True
          | x == "ecl" = True
          | x == "pid" = True
          | otherwise = False

-- the splitOn here will group thing by blank lines, since the input can be across multiple lines
solve :: String -> Int
solve = length . filter id . map isValid . map unwords . splitOn "" . lines

main :: IO ()
main = interact $ show . solve
