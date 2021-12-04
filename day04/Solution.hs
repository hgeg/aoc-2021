module Main where

--import modules here
import Data.List (transpose, delete, null)
--

-- solution types
data Board = Board {
    rows :: [[String]],
    cols :: [[String]]
} deriving Eq

empty :: Board
empty = Board [] []

mkBoard :: [[String]] -> Board
mkBoard rows = Board rows cols
    where cols = transpose rows

markNumber :: String -> Board -> Board
markNumber s (Board r c) = Board (map (delete s) r) (map (delete s) c)

doesWin :: Board -> Bool
doesWin (Board r c) = not . null . filter null $ r ++ c

instance Show Board where
    show (Board rows _) = "\n" ++ (unlines . map unwords $ rows)
--

-- define input and output per problem
type Input  = ([String], [Board])
type Output = Int
--

--begin part 1
part1 :: Part
part1 = uncurry (*) . uncurry runBingo
    where runBingo :: [String] -> [Board] -> (Int, Int)
          runBingo []     _      = (0,0)
          runBingo (n:ns) boards = let marked = map (markNumber n) $ boards
                                       winning = filter doesWin marked
                                   in case safeHead winning of
                                        (Just b) -> (read n, sum . map (sum . map read) . rows $ b)
                                        _        -> runBingo ns marked
          safeHead :: [a] -> Maybe a
          safeHead [] = Nothing
          safeHead (x:xs) = Just x
--end

--begin part 2
part2 :: Part
part2 = uncurry (*) . uncurry runBingo
    where runBingo :: [String] -> [Board] -> (Int, Int)
          runBingo []     _   = (0,0)
          runBingo (n:ns) boards = let marked = map (markNumber n) $ boards
                                       winning = filter doesWin marked
                                       playing = filter (not . doesWin) marked
                                   in case safeHead winning of
                                        (Just b) -> if length boards == 1
                                                        then (read n, sum . map (sum . map read) . rows $ b)
                                                        else runBingo ns playing
                                        _        -> runBingo ns marked
          safeHead :: [a] -> Maybe a
          safeHead [] = Nothing
          safeHead (x:xs) = Just x
--end

-- do not touch
type Part = Input -> Output

readInput :: IO (Input)
readInput = (decode . lines) <$> readFile "input"
    where decode (ns:"":bs) = (readNumbers ns, readBoards bs)
          decode n            = error $ "cannot parse the input: " ++ show n
          readNumbers :: String -> [String]
          readNumbers s = case break (==',') s of
                            (a, ',':b) -> a : readNumbers b
                            (a, "")    -> [a]
          readBoards :: [String] -> [Board]
          readBoards [] = []
          readBoards ("":ns) = readBoards ns
          readBoards ns = let (m, rest) = break (=="") ns
                          in (mkBoard . map words $ m) : readBoards rest

run :: Part -> Input -> IO ()
run = (.) (putStrLn . show)

main :: IO ()
main = input >>= run part1
    >> input >>= run part2
    where input = readInput
