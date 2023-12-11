import Data.List.Split
import Data.List

-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
-- count winning numbers: n
-- card gets 2^(n-1) points
-- sum up all cards
-- intersect to count winning numbers

main = do
  contents <- readFile "input.txt"
  let l = lines contents
  print (sum $ map processCard l)

splitCard :: String -> [String]
splitCard s = [(splitOn ":" (tmp !! 0)) !! 1, tmp !! 1]
  where
    tmp = (splitOn "|" s) -- [Card 1: 41 48 83 86 17, 83 86  6 31 17  9 48 53]

parseNumbers :: String -> [Int]
parseNumbers inp = map (\s -> read s :: Int) $ filter (\s -> s /= "") (splitOn " " $ inp)

processCard :: String -> Int
processCard c = case ourWinning of
                  0 -> 0
                  _ -> 2 ^ (ourWinning-1)
  where
    winningNums = parseNumbers $ splitCard c !! 0
    ourNums = parseNumbers $ splitCard c !! 1
    ourWinning = length $ intersect winningNums ourNums




