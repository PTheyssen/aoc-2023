import Data.List.Split
import Data.List
import Data.Stack
import qualified Data.Map as M

-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
-- count winning numbers: n
-- win copy of n next cards so if n = 4 then copies of cards 2,3,4,5

main = do
  contents <- readFile "input.txt"
  let input = zip [1..] $ lines contents
  -- print $ take 5 l
  let dict = M.fromList $ map (\l -> (fst l, winningNumOnCard l)) input
  -- print dict
  let result = sum $ map (\e -> theFun dict (fst e)) input
  print result


-- numInstances
-- starts with num of cards
-- whenever create new one increment
splitCard :: String -> [String]
splitCard s = [(splitOn ":" (tmp !! 0)) !! 1, tmp !! 1]
  where
    tmp = (splitOn "|" s) -- [Card 1: 41 48 83 86 17, 83 86  6 31 17  9 48 53]

parseNumbers :: String -> [Int]
parseNumbers inp = map (\s -> read s :: Int) $ filter (\s -> s /= "") (splitOn " " $ inp)

winningNumOnCard :: (Int, String) -> [Int]
winningNumOnCard (ind, c) = [ind+1..ind+ourWinning]
  where
    winningNums = parseNumbers $ splitCard c !! 0
    ourNums = parseNumbers $ splitCard c !! 1
    ourWinning = length (intersect winningNums ourNums)


testDict :: M.Map Int [Int]
testDict = M.fromList [(1, [2,3,4,5]), (2, [3,4]), (3, [4,5]), (4, [5]), (5, []), (6, [])]


theFun :: M.Map Int [Int] -> Int -> Int
theFun dict x = 1 + (sum $ map (\y -> theFun dict y) (dict M.! x))
