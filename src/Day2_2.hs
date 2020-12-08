import Data.List.Split

validPassword :: [String] -> Bool
validPassword [policy,letter,password] =
  let (pos1, pos2) = positions policy
      l = head letter
   in (password !! pos1 == l) /= (password !! pos2 == l)
  where
    between min max x = x <= max && x >= min
    positions = (\(x:y:_) -> (read x - 1, read y - 1) :: (Int, Int)) . splitOn "-"
validPassword _ = False

solve :: String -> Int
solve = length . filter (== True) . map (validPassword . words) . lines

main :: IO ()
main = interact $ show . solve
