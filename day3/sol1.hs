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
import qualified Text.Parsec as Parsec


main = do
  contents <- readFile "input.txt"
  let l = lines contents
  let rows = zip [0..] l
  print rows

test p = Parsec.parse p ""
parse rule text = Parsec.parse rule "(source)" text

rowParser :: Parsec.Parsec String Int [(Int, Char)]
rowParser = do
  row <- (Parsec.many1 entryParser)
  return row

-- parse with state
entryParser :: Parsec.Parsec String Int (Int, Char)
entryParser = do
  c <- (Parsec.noneOf ['0'..'9']) Parsec.<|> (Parsec.digit)
  index <- Parsec.getState  
  Parsec.modifyState (+1)
  return (index, c)


l1 = ".......*563......*........................349.....945.................+......@........*....964%......33.642...431.......*............968...."

