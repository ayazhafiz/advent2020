import           Text.Parsec             hiding ( Empty )
import           Text.Parsec.String             ( Parser )

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
  in  (string "N" >> North <$> parseUnsigned)
        <|> (string "S" >> South <$> parseUnsigned)
        <|> (string "E" >> East <$> parseUnsigned)
        <|> (string "W" >> West <$> parseUnsigned)
        <|> (string "L" >> RotL . flip div 90 <$> parseUnsigned)
        <|> (string "R" >> RotR . flip div 90 <$> parseUnsigned)
        <|> (string "F" >> Fwd <$> parseUnsigned)

rotl :: Int -> Position -> Position
-- (1, 2) -> (-2, 1) -> (-1, -2) -> (2, -1) -> (1, 2) -> ...
rotl 0 p        = p
rotl n p@(x, y) = if n >= 4 then rotl (n `mod` 4) p else rotl (n - 1) (-y, x)

rotr :: Int -> Position -> Position
rotr n = rotl (4 - (n `mod` 4))

route1 :: [Command] -> Position
route1 cmds =
  let step :: (Position, Position) -> Command -> (Position, Position)
      step ((x, y), dir         ) (North by) = ((x, y + by), dir)
      step ((x, y), dir         ) (South by) = ((x, y - by), dir)
      step ((x, y), dir         ) (East  by) = ((x + by, y), dir)
      step ((x, y), dir         ) (West  by) = ((x - by, y), dir)
      step (p     , dir         ) (RotL  by) = (p, rotl by dir)
      step (p     , dir         ) (RotR  by) = (p, rotr by dir)
      step ((x, y), dir@(dx, dy)) (Fwd   by) = ((x + by * dx, y + by * dy), dir)
  in  fst $ foldl step ((0, 0), (1, 0)) cmds

route2 :: [Command] -> Position
route2 cmds =
  let step :: (Position, Position) -> Command -> (Position, Position)
      step (p     , (wx, wy)   ) (North by) = (p, (wx, wy + by))
      step (p     , (wx, wy)   ) (South by) = (p, (wx, wy - by))
      step (p     , (wx, wy)   ) (East  by) = (p, (wx + by, wy))
      step (p     , (wx, wy)   ) (West  by) = (p, (wx - by, wy))
      step (p     , wp         ) (RotL  by) = (p, rotl by wp)
      step (p     , wp         ) (RotR  by) = (p, rotr by wp)
      step ((x, y), wp@(wx, wy)) (Fwd   by) = ((x + by * wx, y + by * wy), wp)
  in  fst $ foldl step ((0, 0), (10, 1)) cmds

l1 :: Position -> Int
l1 (x, y) = abs x + abs y

main = do
  input <- lines <$> readFile "input.txt"
  let Right cmds = mapM (parse parseCommand "") input
  print "Part 1:"
  print $ l1 $ route1 cmds
  print "Part 2:"
  print $ l1 $ route2 cmds
