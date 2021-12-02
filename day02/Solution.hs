module Main where

data Input = Forward Int | Up Int | Down Int
type Part a = [Input] -> a

part1 :: Part Int
part1 = move 0 0
    where move pos depth ((Forward h):cs) = move (pos + h) depth cs
          move pos depth ((Down v):cs)    = move pos (depth + v) cs
          move pos depth ((Up v):cs)      = move pos (depth - v) cs
          move pos depth []               = pos * depth

part2 :: Part Int
part2 = move 0 0 0
    where move pos depth aim ((Down v):cs)    = move pos depth (aim+v) cs
          move pos depth aim ((Up v):cs)      = move pos depth (aim-v) cs
          move pos depth aim ((Forward h):cs) = move (pos+h) (depth+aim*h) aim cs
          move pos depth aim []               = pos * depth

readInput :: IO ([Input])
readInput = (map (decode . words) . lines) <$> readFile "input"
    where decode ["forward", v] = Forward . read $ v
          decode ["up"     , v] = Up      . read $ v
          decode ["down"   , v] = Down    . read $ v

run :: Show a => (Part a) -> [Input] -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
