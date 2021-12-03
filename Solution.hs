module Main where

--import modules here
import Data.Void
--

-- solution types
-- define input and output per problem
type Input  = Void
type Output = Void
--

--begin part 1
part1 :: Part
part1 = undefined
--end

--begin part 2
part2 :: Part
part2 = undefined
--end

-- do not touch
type Part = Input -> Output

readInput :: IO (Input)
readInput = (decode . lines) <$> readFile "input"
    where decode = undefined

run :: Part -> Input -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
