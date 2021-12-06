module Main where

import Data.IntMap.Strict (IntMap, Key)
import qualified Data.IntMap.Strict as Map

-- solution types
-- define input and output per problem
type Input  = [Int]
type Output = Int

type Generator = IntMap Int
--

--begin part 1
part1 :: Part
part1 = simulate 80 . mkGenerator 80

mkGenerator :: Int -> [Int] -> Generator
mkGenerator _ []     = Map.empty
mkGenerator d (x:xs) = incrementBy (d-x) 1 $ mkGenerator d xs

incrementBy :: Key -> Int -> Generator -> Generator
incrementBy k v' gen = case Map.lookup k gen of
        Nothing -> Map.insert k v'     gen
        Just v  -> Map.insert k (v+v') gen

simulate :: Int -> Generator -> Int
simulate 0 gen = foldr (+) 0 gen
simulate n gen = case Map.lookup n gen of
    Nothing -> simulate (pred n) gen
    Just v  -> simulate (pred n)
                . incrementBy (n-9) v
                . incrementBy (n-7) v
                . incrementBy (n) (-v)
                $ gen
--end

--begin part 2
part2 :: Part
part2 = simulate 256 . mkGenerator 256
--end

-- do not touch
type Part = Input -> Output

readInput :: IO (Input)
readInput = decode <$> readFile "input"
    where decode = map read . words . map (\c -> if c==',' then ' ' else c)

run :: Part -> Input -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
