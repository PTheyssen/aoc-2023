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
