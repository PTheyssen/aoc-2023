import Data.List
import Data.List.Split
import Debug.Trace

main = do
  contents <- readFile "input.txt"
  let l = lines contents
  let seeds = drop 1 $ splitOn " " (head $ take 1 l)
  let allSeeds = generateSeeds seeds
  print $ length allSeeds
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
            allSeeds
  let sol = minimum locations
  print sol


-- TODO to inefficient => need to operate on intervals and then only generate final interval
generateSeeds :: [String] -> [String]
generateSeeds s = concat $ map (\p -> fun (p!!0) (p!!1))  $ chunksOf 2 s
                  where
                    fun s1 s2 =
                      let i1 = read s1 :: Int in
                        let i2 = read s2 :: Int in
                          map show [i1..i1+i2-1]

-- process 2 at a time 

parseMap :: String -> [String] -> [String]
parseMap s = takeWhile (/= "") . dropWhile (/= s)

parseMap' :: String -> [String] -> (String -> String)
parseMap' name lines = applyMap $ createMapFunction (parseMap name lines)

createMapFunction :: [String] -> [String -> Maybe String]
createMapFunction s = map (\l -> fun (l !! 0, l !! 1, l !! 2)) ranges
  where
    ranges = map (map (\x -> read x :: Int) . splitOn " ") (tail s)
    fun :: (Int, Int, Int) -> (String -> Maybe String)
    fun (dest, source, len) x =
      let num = read x :: Int
       in ( if source <= num && (source + len) >= num
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
