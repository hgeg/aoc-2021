module Main where

type Part a = [String] -> a

part1 :: Part Int
part1 measurements = length               -- count
                   . filter (uncurry (<)) -- filter the increasing pairs
                   . zip ms $ tail ms     -- pair the measurements
    where ms = read <$> measurements :: [Int]

part2 :: Part Int
part2 measurements = part1 . map show $ windowSums -- use naive measurement from part 1
    where windowSums = sum3 <$> (zip3 ms (tail ms) (tail.tail $ ms)) -- take the sum of 3-pairs from input
          sum3 (a,b,c) = a+b+c
          ms = read <$> measurements :: [Int]

readInput :: IO ([String])
readInput = lines <$> readFile "input"

run :: Show a => (Part a) -> [String] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
