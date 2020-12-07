import Data.List.Split

validPassword :: [String] -> Bool
validPassword [policy,letter,password] =
  let (min, max) = minmax policy
      l = head letter
   in between min max $ length (filter (== l) password)
  where
    between min max x = x <= max && x >= min
    minmax = (\(x:y:_) -> (read x, read y) :: (Int, Int)) . splitOn "-"
validPassword _ = False

solve :: String -> Int
solve = length . filter (== True) . map (validPassword . words) . lines

main :: IO ()
main = interact $ show . solve
