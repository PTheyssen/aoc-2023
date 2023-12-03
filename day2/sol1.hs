import qualified Text.Parsec as Parsec

data Cube =
  Red | Blue | Green deriving Show

parse rule text = Parsec.parse rule "(source)" text

main = do
  contents <- readFile "input.txt"
  let l = lines contents
  let b = filter isValidGame $ map (parse gameParser) l
  let solution = sum $ map (\g -> case g of
                                    Left _ -> 0
                                    Right tup -> fst tup) b
  print solution


test p = Parsec.parse p ""

isValidGame :: Either Parsec.ParseError (Int, [[(Cube, Int)]]) -> Bool
isValidGame p = case p of
                  Left _ -> False
                  Right (id, c) -> all (\(c, num) -> case c of
                                           Red -> num <= 12
                                           Blue -> num <= 14
                                           Green -> num <= 13) (concat c)


cubeParser :: Parsec.Parsec String () (Cube, Int)
cubeParser = do
  Parsec.optional Parsec.spaces
  num <- Parsec.many1 Parsec.digit
  Parsec.optional Parsec.spaces
  cube <- Parsec.choice [(Parsec.string "red"),
                         (Parsec.string "blue"),
                         (Parsec.string "green")]
  Parsec.optional Parsec.spaces
  _ <- Parsec.optional (Parsec.oneOf ",")
  case cube of
    "red" -> return (Red, read num :: Int)
    "blue" -> return (Blue, read num :: Int)
    "green" -> return (Green, read num :: Int)
    _ -> return (Red, -1)

drawParser :: Parsec.Parsec String () [(Cube, Int)]
drawParser = do
  draw <- Parsec.many1 cubeParser
  Parsec.skipMany (Parsec.char ';')
  return draw


gameParser :: Parsec.Parsec String () (Int, [[(Cube, Int)]])
gameParser = do
  _ <- Parsec.string "Game "
  id <- Parsec.many1 Parsec.digit
  _ <- Parsec.char ':'
  draws <- Parsec.many1 drawParser
  -- parse draws separately list of
  return (read id :: Int, draws)
