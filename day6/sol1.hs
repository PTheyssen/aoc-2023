import Data.List.Split


main = do
  contents <- readFile "input.txt"
  let l = lines contents
  let tmp = map (filter ((/=) "")) $ map (splitOn " ") l
  let times = tail $ tmp !! 0
  let distances = tail $ tmp !! 1
  let races = zip (map (\x -> read x :: Int) times) (map (\x -> read x :: Int) distances)
  let solutions =
        product $
        map (\(x,y) -> y-x+1) $
        map (\(x,y) -> (ceiling x, floor y)) $
        map (\(t,d) -> solveQuadratic t d) races -- [(2,5),...]
  print solutions        


solveQuadratic :: Int -> Int -> (Double, Double)
solveQuadratic time dist = case x1 < x2 of
                             True -> (x1, x2)
                             False -> (x2, x1)
  where
    a = -1
    b = (fromIntegral time)
    c = - (fromIntegral dist)
    delta = b ** 2 - 4 * a * c
    x1 = (-b - sqrt delta) / (2 * a)
    x2 = (-b + sqrt delta) / (2 * a)
