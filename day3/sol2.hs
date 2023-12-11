import Text.Read
import Data.Matrix
import Data.List.GroupBy

main = do
  contents <- readFile "input.txt"
  let l = lines contents
  let x = transformList $ toList $ fillDigits $ fillDigits $ fillDigits (filterDigits (fromLists l))
  let digits = map (\d -> read d :: Int) $ filter (\s -> (length s) > 0) x
  print $ sum digits


-- need gear ratio * that is next two digits exactly
filterGears :: Matrix Char -> Matrix (Int, [(Int, Int)])
filterGears m = mapPos (\(r,c) e -> case checkDigit (r,c) e m of
                                       True -> case readMaybe [e] :: Maybe Int of
                                                 Nothing -> (-1, False)
                                                 Just d -> (d, True)
                                       False -> case readMaybe [e] :: Maybe Int of
                                                 Nothing -> (-1, False)
                                                 Just d -> (d, False)
                                       ) m
  where
    checkDigit (r,c) e m =
      let pos = [(r-1, c-1),
                 (r-1, c),
                 (r-1, c+1),
                 (r+1, c),
                 (r+1, c+1),
                 (r+1, c-1),
                 (r, c+1),
                 (r, c-1)] in
        let validPos = filter (\(r,c) -> (r > 0 && r <= nrows m) && (c > 0 && c <= ncols m)) pos
        in
          any (\(r,c) -> isSymbol (m ! (r,c))) validPos


fillDigits :: Matrix (Int, Bool) -> Matrix (Int, Bool)
fillDigits m = mapPos (\(r,c) e -> case e of
                                     ((-1), _) -> e
                                     (x, False) -> (fst e, checkOthers (r,c) e m)
                                     (y, True) -> e
                                     ) m
  where
    checkOthers (r,c) e m =
      let pos = [(r, c-1),
                 (r, c+1)] in
        let validPos = filter (\(r,c) -> (r > 0 && r <= nrows m) && (c > 0 && c <= ncols m)) pos
                in
          any (\(r,c) -> snd (m ! (r,c))) validPos


isSymbol :: Char -> Bool
isSymbol c  = c `notElem` (['0'..'9'] ++ ['.'])

transformList :: [(Int, Bool)] -> [String]
transformList = map (concatMap (show . fst) . filter snd) . groupBy (\(_, a) (_, b) -> a == b)

