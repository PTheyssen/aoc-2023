import Data.List
import Data.List.Split
import Debug.Trace

main = do
  contents <- readFile "input.txt"
  let l = lines contents
  let seeds = drop 1 $ splitOn " " (head $ take 1 l)
  let seedToSoilMap = parseMap' "seed-to-soil map:" l
  let soilToFertilizerMap = parseMap' "soil-to-fertilizer map:" l
  let fertilizerToWaterMap = parseMap' "fertilizer-to-water map:" l
  let waterToLightMap = parseMap' "water-to-light map:" l
  let lightToTempMap = parseMap' "light-to-temperature map:" l
  let tempToHumidityMap = parseMap' "temperature-to-humidity map:" l
  let humidityToLocationMap = parseMap' "humidity-to-location map:" l
  let locations =
        map (\x -> read x :: Int) $
          map
            ( humidityToLocationMap
                . tempToHumidityMap
                . lightToTempMap
                . waterToLightMap
                . fertilizerToWaterMap
                . soilToFertilizerMap
                . seedToSoilMap
            )
            seeds
  let sol = minimum locations
  print sol
  -- let seed = seeds !! 1
  -- print seed
  -- print $ seedToSoilMap seed
  -- print $ (soilToFertilizerMap . seedToSoilMap) seed

-- print $ (seedToSoilMap. soilToFertilizerMap . fertilizerToWaterMap) seed
-- print $ (seedToSoilMap. soilToFertilizerMap . fertilizerToWaterMap . waterToLightMap) seed
-- print $ (seedToSoilMap. soilToFertilizerMap . fertilizerToWaterMap . waterToLightMap . lightToTempMap) seed
-- print $ (seedToSoilMap. soilToFertilizerMap . fertilizerToWaterMap
-- . waterToLightMap . lightToTempMap . tempToHumidityMap) seed
-- print $ (seedToSoilMap. soilToFertilizerMap . fertilizerToWaterMap
-- . waterToLightMap . lightToTempMap . tempToHumidityMap . humidityToLocationMap) seed

parseMap :: String -> [String] -> [String]
parseMap s = takeWhile (/= "") . dropWhile (/= s)

parseMap' :: String -> [String] -> (String -> String)
parseMap' name lines = applyMap $ createMapFunction (parseMap name lines)

-- soilToFertilizerMap = parseMap "seed-to-soil map:" testin

-- parseSoilToFertilizer :: [String] -> [String]
-- .parseSoilToFertilizer = takeWhile (/= "") . dropWhile (/= "seed-to-soil map:")

createMapFunction :: [String] -> [String -> Maybe String]
createMapFunction s = map (\l -> fun (l !! 0, l !! 1, l !! 2)) ranges
  where
    ranges = map (map (\x -> read x :: Int) . splitOn " ") (tail s)
    fun :: (Int, Int, Int) -> (String -> Maybe String)
    fun (dest, source, len) x =
      let num = read x :: Int
       in ( if source <= num && (source + len) >= num
              -- then trace ("(num, source, len, dest, result)" ++ show (num, source, len, dest, (num - source + dest))) Just $ show (num - source + dest)
              then Just $ show (num - source + dest)                   
              else Nothing
          )

applyMap :: [String -> Maybe String] -> (String -> String)
applyMap m =
  ( \inp -> case filter (/= Nothing) (map (\f -> f inp) m) of
      [] -> inp
      l -> case (head l) of
        Nothing -> inp
        (Just x) -> x
  )

-- 74 - 64 +
-- 64 68
-- 65 69
-- ...
-- 74 78
-- ...
-- 77 81

testin =
  [ "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  ]
