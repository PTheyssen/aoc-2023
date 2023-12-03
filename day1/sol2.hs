{-# LANGUAGE OverloadedStrings #-}
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read -- readMaybe
import qualified Data.Text as T
import Data.Text.Internal.Search

main = do
  contents <- readFile "input.txt"
  -- print $ map getCalibrationValue' $ (lines (contents))
  print $ sum $ map (getCalibrationValue'. T.pack) $ (lines (contents))

getCalibrationValue' :: T.Text -> Int
getCalibrationValue' s = read d :: Int
  where
    d = concat $ map show $ getCalibrationValue s

x1 = getCalibrationValue' "fourthreetpmqqtzgtwofour"
x2 = getCalibrationValue' "1fourthreetpmqqtzgtwofour"
s1 = findSpelledOutDigits  "1fourthreetpmqqtzgtwofour"

getCalibrationValue :: T.Text -> [Int]
getCalibrationValue s =
  case length listOfDigits of
    1 -> listOfDigits ++ listOfDigits
    2 -> listOfDigits
    _ -> [(head listOfDigits)] ++ [last listOfDigits]
  where
    listOfDigits = concat $ map t $ split (oneOf ['1'..'9']) (T.unpack s) -- list of
    t = \x -> case readMaybe x :: Maybe Int of
                Just d -> [d]
                Nothing -> findSpelledOutDigits (T.pack x)
           

-- part 2 digits spelled out in letters
-- need to split on digits => check if words in between
findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)


compFirstElem :: (Int, Int) -> (Int, Int) -> Ordering
compFirstElem (x1, _) (x2, _) = compare x1 x2

findSpelledOutDigits :: T.Text -> [Int]
findSpelledOutDigits s =
  map snd $ sortBy compFirstElem $ concat $ map (\x -> tmp s x) spelledOut
  -- combine and order
  where
    spelledOut = [("one", 1), ("two", 2), ("three", 3),
                  ("four", 4), ("five", 5), ("six", 6),
                  ("seven", 7), ("eight", 8), ("nine", 9)]

-- findString needs all occurences!
tmp :: T.Text -> (T.Text, Int) -> [(Int, Int)]
tmp s tup = case indices (fst tup) s of
              x -> map (\y -> (y, snd tup)) x

