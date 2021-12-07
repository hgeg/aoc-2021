module Main where

--import modules here
import Data.List
--

-- solution types
-- define input and output per problem
type Input  = [Int]
type Output = Int
--

--begin part 1
part1 :: Part
part1 crabs = minimum fuels
   where fuels :: [Int]
         fuels = foldr (zipWith (+)) (repeat 0)
               . map (\n -> map (abs . (-)n) [minPos..maxPos])
               $ crabs
         minPos = minimum crabs
         maxPos = maximum crabs
--end

--begin part 2
part2 :: Part
part2 crabs = minimum fuels
   where fuels :: [Int]
         fuels = foldr (zipWith (+)) (repeat 0)
               . map (\n -> map (gsum . abs . (-) n) [minPos..maxPos])
               $ crabs
         gsum n = (n * (n+1)) `div` 2
         minPos = minimum crabs
         maxPos = maximum crabs
--end

-- do not touch
type Part = Input -> Output

readInput :: FilePath -> IO (Input)
readInput = fmap decode . readFile

decode :: String -> Input
decode = map read . words . map (\c -> if c==',' then ' ' else c)

run :: Part -> Input -> IO ()
run = (.) (putStrLn . show)

test :: IO()
test = input >>= run part1
    >> input >>= run part2
    where input = readInput "example"

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput "input"
