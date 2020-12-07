import Data.Set (Set)
import qualified Data.Set as Set

-- We return the min since there should only be one anyway
solveHelper :: Set Int -> Int
solveHelper s = let x = Set.findMin $ Set.filter (\x -> Set.member (2020 - x) s) s
           in x * (2020 - x)

solve :: String -> Int
solve = solveHelper . Set.fromList . map read . lines

main :: IO ()
main = interact $ show . solve
