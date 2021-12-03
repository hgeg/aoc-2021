module Main where

import Data.List ((!!), transpose)

type Input = [Binary]
type Output = Int
type Part = Input -> Output

-- binary representation
type Binary = [Bit]
data Bit = O | Z deriving Eq

-- flipping helps with converting most common
-- to least common bit and vice versa
flipBit :: Bit -> Bit
flipBit O = Z
flipBit Z = O

toDecimal :: [Bit] -> Int
toDecimal bits = go len bits
    where len = 2^(length bits-1)
          go m (O:bs) = go (m `div` 2) bs + m
          go m (Z:bs) = go (m `div` 2) bs
          go _ []     = 0

mostCommonBits :: [Binary] -> Binary
mostCommonBits = map (\a -> if a>=0 then O else Z)
               . map (foldr (\a b -> if a==O then b+1 else b-1) 0)
               . transpose -- work column by column

part1 :: Part
part1 nums = toDecimal gamma * toDecimal epsilon
    where gamma = mostCommonBits nums
          epsilon = flipBit <$> gamma

part2 :: Part
part2 nums = toDecimal (generatorRating 0 nums) * toDecimal (scrubberRating 0 nums)
    where generatorRating :: Int -> [Binary] -> Binary
          generatorRating _ [n] = n
          generatorRating p ns  = let mcb = (mostCommonBits ns)!!p
                                      remaining = filter (\n -> (n!!p)==mcb) ns
                                  in generatorRating (p+1) remaining
          scrubberRating :: Int -> [Binary] -> Binary
          scrubberRating _ [n] = n
          scrubberRating p ns  = let lcb = flipBit $ (mostCommonBits ns)!!p
                                     remaining = filter (\n -> (n!!p)==lcb) ns
                                 in scrubberRating (p+1) remaining

readInput :: IO (Input)
readInput = (map decode . lines) <$> readFile "input"
    where decode :: String -> Binary
          decode ('0':bs) = Z : decode bs
          decode ('1':bs) = O : decode bs
          decode []       = []
          decode _        = error "Parse error: invalid input character"

run :: Part -> Input -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
