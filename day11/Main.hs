import           Text.Parsec             hiding ( Empty )
import           Text.Parsec.String             ( Parser )

data Seat = Occupied | Empty | Floor
  deriving Eq

instance Show Seat where
  show Occupied = "#"
  show Empty    = "L"
  show Floor    = "."

parseSeat :: Parser Seat
parseSeat =
  (string "#" >> return Occupied)
    <|> (string "L" >> return Empty)
    <|> (string "." >> return Floor)

parseSeatRow :: Parser [Seat]
parseSeatRow = many parseSeat

occupied :: Seat -> Bool
occupied Occupied = True
occupied _        = False

nOccupied :: Seat -> Int
nOccupied = fromEnum . occupied

countOccupied :: [[Seat]] -> Int
countOccupied = sum . map (length . filter occupied)

inBounds :: [[a]] -> Int -> Int -> Bool
inBounds list row col =
  row >= 0 && col >= 0 && row < length list && col < length (head list)

type OccupiedAround = [[Seat]] -> Int -> Int -> Int
type MapSeat = Int -> Seat -> Seat

stepSeating :: OccupiedAround -> MapSeat -> [[Seat]] -> [[Seat]]
stepSeating occupiedAround mapSeat seats = zipWith
  (\r row -> zipWith (mapSeat . occupiedAround seats r) [0 ..] row)
  [0 ..]
  seats


fix :: Eq a => (a -> a) -> a -> a
fix f x = let x' = f x in if x == x' then x else fix f x'


main = do
  input <- lines <$> readFile "input.txt"
  -- Using a list is a terrible idea, all indexing operations are O(n)... is
  -- there a better way? Maybe a map?
  let Right plane = mapM (parse parseSeatRow "") input

  print "Part 1:"
  let part1Occupied =
        let occupiedAround :: OccupiedAround
            occupiedAround seats r c =
                let surrounding =
                        [ (seats !! nr) !! nc
                        | nr <- [r - 1 .. r + 1]
                        , nc <- [r - 1 .. r + 1]
                        , (nr, nc) /= (r, c)
                        , inBounds seats nr nc
                        ]
                in  sum (map nOccupied surrounding)
            mapSeat :: MapSeat
            mapSeat 0   Empty    = Occupied
            mapSeat num Occupied = if num >= 4 then Empty else Occupied
            mapSeat _   seat     = seat
            fixedPlane = fix (stepSeating occupiedAround mapSeat) plane
        in  countOccupied fixedPlane
  print part1Occupied

  print "Part 2:"
  let part2Occupied =
        let occupiedAround :: OccupiedAround
            occupiedAround seats r c =
                let surrounding =
                        [ map (\mul -> (r + mul * dr, c + mul * dc)) [1 ..]
                        | dr <- [-1 .. 1]
                        , dc <- [-1 .. 1]
                        , (dr, dc) /= (0, 0)
                        ]
                in  let searchSeats :: [(Int, Int)] -> Int
                        searchSeats ((r, c) : rest) = if not (inBounds seats r c)
                          then 0
                          else case (seats !! r) !! c of
                            Empty    -> 0
                            Occupied -> 1
                            Floor    -> searchSeats rest
                    in  sum (map searchSeats surrounding)
            mapSeat :: MapSeat
            mapSeat 0   Empty    = Occupied
            mapSeat num Occupied = if num >= 5 then Empty else Occupied
            mapSeat _   seat     = seat
            fixedPlane = fix (stepSeating occupiedAround mapSeat) plane
        in  countOccupied fixedPlane
  print part2Occupied
