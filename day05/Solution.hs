module Main where

--import modules here
import Data.List
import qualified Data.Map as Map
--

-- solution types
-- define input and output per problem
type Input  = [Line]
type Output = Int

type Point = (Int, Int)
data Line = Line { start :: Point, end :: Point } deriving (Eq, Ord)

instance Show Line where
    show (Line s e) = show s ++ "->" ++ show e
--

--begin part 1
runLine :: Line -> [Point]
runLine (Line (x1,y1) (x2,y2))
            | x1 == x2            = [(x1, y) | y<-[min y1 y2 .. max y1 y2]]
            | y1 == y2            = [(x, y1) | x<-[min x1 x2 .. max x1 x2]]
            | otherwise           = zip [x1,(x1+(signum (x2-x1)))..x2] [y1,(y1+(signum (y2-y1)))..y2]

part1 :: Part
part1 = part2 . filter isHorizontalOrVertical
    where isHorizontalOrVertical (Line (x1,y1) (x2,y2)) = x1==x2 || y1==y2
--end

--begin part 2
part2 :: Part
part2 = length . filter ((>1) . length) . group . sort . concatMap runLine
--end

-- do not touch
type Part = Input -> Output

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

readInput :: IO (Input)
readInput = (decode . lines) <$> readFile "input"
    where decode = map parseLine

parseLine :: String -> Line
parseLine = go ""
    where go :: String -> String -> Line
          go f (' ':'-':'>':' ':xs) = Line (mkPoint f) (mkPoint xs)
          go f (x:xs)      = go (f++[x]) xs
          mkPoint p = let v = split ',' p in (read . head $ v, read . last $ v)

run :: Part -> Input -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput