import Data.Sequence (Seq (Empty, (:<|), (:|>)),  (<|), (|>))
import qualified Data.Sequence as Seq

targetSum :: Int
targetSum = 2020

solveHelper :: Seq Int -> Int
solveHelper (x :<| xs) =
  case xs of
    xs' :|> y -> map (\z -> (targetSum - x - y - z, x * y * z)) xs'
     -- case compare (x + y + z) targetSum of
     --             LT -> solveHelper $ xs' |> y
     --             GT -> solveHelper $ x <| xs'
     --             EQ -> x * y

solve :: String -> Int
solve = solveHelper . Seq.sort . Seq.fromList . map read . lines

main :: IO ()
main = interact $ show . solve
