
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read -- readMaybe

main = do
  contents <- readFile "input.txt"
  print $ sum $ map getCalibrationValue' $ (lines (contents))

getCalibrationValue' :: String -> Int
getCalibrationValue' s = read d :: Int
  where
    d = concat $ map show $ getCalibrationValue s

a = getCalibrationValue' "two1nine"
b = getCalibrationValue' "eightwothree"
c = getCalibrationValue' "abcone2threexyz"
d = getCalibrationValue' "xtwone3four"
f = getCalibrationValue' "4nineeightseven2"
e = getCalibrationValue' "zoneight234"
g = getCalibrationValue' "7pqrstsixteen"

getCalibrationValue :: String -> [Int]
getCalibrationValue s =
  case length listOfDigits of
    1 -> listOfDigits ++ listOfDigits
    2 -> listOfDigits
    _ -> [(head listOfDigits)] ++ [last listOfDigits]
  where
    listOfDigits = concat $ map t $ split (oneOf ['1'..'9']) s -- list of
    t = \x -> case readMaybe x :: Maybe Int of
                Just d -> [d]
                Nothing -> findSpelledOutDigits x
           

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
