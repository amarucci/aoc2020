floor' = fromInteger . ceiling
ceiling' = fromInteger . ceiling

resolve :: Double -> Double -> [Double] -> Double
resolve min max (0:xs) = resolve min (min + floor' ((max - min) / 2) - 1) xs
resolve min max (1:xs) = resolve (min + ceiling' ((max - min) / 2)) max xs
resolve min max [] = min

row :: String -> Double
row = (resolve 0 8) . map convert
  where convert c = if c == 'L' then 0 else 1

column :: String -> Double
column = (resolve 0 127) . map convert
  where convert c = if c == 'F' then 0 else 1

seatNumber :: String -> Double
seatNumber s = 8 * column (take 8 s) + row (drop 7 s)

solve :: String -> Double
solve = foldl max 0 . map seatNumber . lines

main :: IO ()
main = interact $ show . solve
