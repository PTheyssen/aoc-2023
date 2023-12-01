import Data.List
import Data.Maybe

main = do
  contents <- readFile "input.txt"
  print $ sum $ map getCalibrationValue (lines  (contents))


getCalibrationValue :: String -> Int
getCalibrationValue s = case length digits of
                          1 -> read (digits ++ digits) :: Int
                          2 -> read (digits) :: Int
                          _ -> read [head digits, last digits] :: Int
  where
    digits = filter (\x -> elem x ['1'..'9']) s



-- part 2 digits spelled out in letters
-- need to split on digits => check if words in between
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)


compFirstElem :: (Int, Int) -> (Int, Int) -> Ordering
compFirstElem (x1, _) (x2, _) = compare x1 x2

findSpelledOutDigits :: String -> [Int]
findSpelledOutDigits s =
  map snd $ sortBy compFirstElem $ mapMaybe (\x -> tmp s x) spelledOut
  -- combine and order
  where
    spelledOut = [("one", 1), ("two", 2), ("three", 3),
                  ("four", 4), ("five", 5), ("six", 6),
                  ("seven", 7), ("eight", 8), ("nine", 9)]

tmp :: String -> (String, Int) -> Maybe (Int, Int)
tmp s tup = case findString (fst tup) s of
              Nothing -> Nothing
              Just pos -> Just (pos, snd tup)
