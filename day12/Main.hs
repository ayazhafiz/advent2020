import Text.Parsec hiding (Empty)
import Text.Parsec.String (Parser)

data Command
  = North Int
  | South Int
  | East Int
  | West Int
  | RotL Int
  | RotR Int
  | Fwd Int

type Position = (Int, Int)

parseCommand :: Parser Command
parseCommand =
  let parseUnsigned :: Parser Int
      parseUnsigned = read <$> many1 digit
   in (string "N" >> North <$> parseUnsigned) <|>
      (string "S" >> South <$> parseUnsigned) <|>
      (string "E" >> East <$> parseUnsigned) <|>
      (string "W" >> West <$> parseUnsigned) <|>
      (string "L" >> RotL . flip div 90 <$> parseUnsigned) <|>
      (string "R" >> RotR . flip div 90 <$> parseUnsigned) <|>
      (string "F" >> Fwd <$> parseUnsigned)

rotl :: Int -> Position -> Position
-- (1, 2) -> (-2, 1) -> (-1, -2) -> (2, -1) -> (1, 2) -> ...
rotl 0 p = p
rotl n p@(x, y) =
  if n >= 4
    then rotl (n `mod` 4) p
    else rotl (n - 1) (-y, x)

rotr :: Int -> Position -> Position
rotr n = rotl (4 - (n `mod` 4))

route1 :: [Command] -> Position
route1 cmds =
  let step :: [Command] -> Position -> Position -> Position
      step [] pos _ = pos
      step (North by:rest) (x, y) dir = step rest (x, y + by) dir
      step (South by:rest) (x, y) dir = step rest (x, y - by) dir
      step (East by:rest) (x, y) dir = step rest (x + by, y) dir
      step (West by:rest) (x, y) dir = step rest (x - by, y) dir
      step (RotL by:rest) p dir = step rest p (rotl by dir)
      step (RotR by:rest) p dir = step rest p (rotr by dir)
      step (Fwd by:rest) (x, y) dir@(dx, dy) =
        step rest (x + by * dx, y + by * dy) dir
   in step cmds (0, 0) (1, 0)

route2 :: [Command] -> Position
route2 cmds =
  let step :: [Command] -> Position -> Position -> Position
      step [] pos _ = pos
      step (North by:rest) p (wx, wy) = step rest p (wx, wy + by)
      step (South by:rest) p (wx, wy) = step rest p (wx, wy - by)
      step (East by:rest) p (wx, wy) = step rest p (wx + by, wy)
      step (West by:rest) p (wx, wy) = step rest p (wx - by, wy)
      step (RotL by:rest) p wp = step rest p (rotl by wp)
      step (RotR by:rest) p wp = step rest p (rotr by wp)
      step (Fwd by:rest) (x, y) wp@(wx, wy) =
        step rest (x + by * wx, y + by * wy) wp
   in step cmds (0, 0) (10, 1)

l1 :: Position -> Int
l1 (x, y) = abs x + abs y

main = do
  input <- lines <$> readFile "input.txt"
  let Right cmds = mapM (parse parseCommand "") input
  print "Part 1:"
  print $ l1 $ route1 cmds
  print "Part 2:"
  print $ l1 $ route2 cmds
