{-
467..114..  => (467, [(0,0), (0,1), (0,2)]), (114, [(0,5), (0,6), (0,7)])
...*......  => (symbol, [(1, 3)])
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

check if any coord of a digit is adjacent to a symbol:
(x1, y1) (x2, y2)
=> |x1 - x2| <= 1 && |y1 - y2| <= 1

save symbols in hashtable according to y-coordinate only check symbols
that are  -1, 0, +1 in y
-}

import Text.Read
import Data.Matrix
import Text.Parsec qualified as Parsec

main = do
  contents <- readFile "input.txt"
  let l = lines contents
  -- let rows = zip [0..] l
  print (take 5 l)

test p = Parsec.parse p ""

parse rule = Parsec.parse rule "(source)"

rowParser :: Parsec.Parsec String Int [(Int, Char)]
rowParser = do Parsec.many1 entryParser

-- parse with state
entryParser :: Parsec.Parsec String Int (Int, Char)
entryParser = do
  c <- Parsec.noneOf ['0' .. '9'] Parsec.<|> Parsec.digit
  index <- Parsec.getState
  Parsec.modifyState (+ 1)
  return (index, c)

l1 = ".......*563......*........................349.....945.................+......@........*....964%......33.642...431.......*............968...."

m =
  [ ".......855.442......190..................................969..........520.......59.............................................172..........",
    ".......................-....@...21...........971........................*..............965.......577=..........316..465*169.................",
    "........881.......881....635......*..........*.............%.577.....864.......873.........................742...*...............714..244...",
    ".......*..../..................602......351...423....939.906...*.........899..-..........833..60..%....965...*....309......43......*.*......",
    "....737......294..........321*.......................$.......337....511.*.........58..............305.*.......153.............130.....638..."
  ]

ma = fromLists m

filterDigits :: Matrix Char -> Matrix Int
filterDigits m = mapPos (\(r,c) e -> case checkDigit (r,c) e m of
                                       True -> case readMaybe [e] :: Maybe Int of
                                                 Nothing -> 0
                                                 Just d -> d
                                       False -> 0
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
        let validPos = filter (\(r,c) -> (r > 0 && r < nrows m) && (c > 0 && c < ncols m)) pos
        in
          any (\(r,c) -> isSymbol (m ! (r,c))) validPos
        

isSymbol :: Char -> Bool
isSymbol c  = c `notElem` (['0'..'9'] ++ ['.'])
-- sliding window => perfect parallelizable (image processing kind of)

-- parse into matrix
