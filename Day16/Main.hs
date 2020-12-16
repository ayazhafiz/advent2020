import           Data.List
import           Data.Bifunctor
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

type Ticket = [Int]
newtype Constraint = Constraint [(Int, Int)]
  deriving (Show)
type NamedConstraints = Map String Constraint

parseUnsigned :: Parser Int
parseUnsigned = read <$> many1 digit

parseBounds :: Parser (Int, Int)
parseBounds = do
  lo <- parseUnsigned
  char '-'
  hi <- parseUnsigned
  return (lo, hi)

parseConstraint :: Parser (String, Constraint)
parseConstraint = do
  name <- many1 (lower <|> char ' ')
  string ": "
  bounds <- parseBounds `sepBy` string " or "
  newline
  return (name, Constraint bounds)

parseTicket :: Parser Ticket
parseTicket = do
  vals <- parseUnsigned `sepBy` char ','
  newline
  return vals

parseInput :: Parser (NamedConstraints, Ticket, [Ticket])
parseInput = do
  constraints <- many1 parseConstraint
  newline
  string "your ticket:"
  newline
  yourTicket <- parseTicket
  newline
  string "nearby tickets:"
  newline
  nearbyTickets <- many1 parseTicket
  return (Map.fromList constraints, yourTicket, nearbyTickets)

meetsConstraint :: Int -> Constraint -> Bool
meetsConstraint v (Constraint bounds) =
  any (\(lo, hi) -> lo <= v && v <= hi) bounds

constraintsMet :: Int -> NamedConstraints -> [Constraint]
constraintsMet val constraints =
  filter (meetsConstraint val) (Map.elems constraints)

scanningErrorRate :: NamedConstraints -> [Ticket] -> Int
scanningErrorRate constraints =
  sum . map (sum . filter (\e -> null (constraintsMet e constraints)))

validTickets :: NamedConstraints -> [Ticket] -> [Ticket]
validTickets constraints =
  filter (not . any (\e -> null (constraintsMet e constraints)))

-- Rotates tickets from a form like
--      C1 C2 C3 C4
--   T1 A  B  C  D
--   T2 E  F  G  H
-- to
--      T1 T2
--   C1 A  E
--   C2 B  F
--   C3 C  G
--   C4 D  H
rotateTickets :: [Ticket] -> [[Int]]
rotateTickets []       = []
rotateTickets ([] : _) = []
rotateTickets l        = map head l : rotateTickets (map tail l)

mfind a mp = case Map.lookup a mp of
  Just b -> b

-- Returns a list of constraints that are valid for all values in a ticket column.
validForColumn :: NamedConstraints -> [Int] -> [String]
validForColumn constraints values = Map.keys $ Map.filter
  (\constraint -> all (`meetsConstraint` constraint) values)
  constraints

-- For each column of ticket values, return a list of valid constraints for that column.
columnOptions :: NamedConstraints -> [[Int]] -> [[String]]
columnOptions constraints = map (validForColumn constraints)

-- Given a list of valid constraints for each ticket column, finds a permutation
-- of exactly one constraint per column satisfying each column's constraint options.
solve :: [[String]] -> [String]
solve options =
  let withColumn = zip [0 ..] options
      sortByConstraints opts = sortOn (length . snd) opts
      solve' :: [(Int, [String])] -> [(Int, String)]
      solve' [] = []
      solve' ((id, [solved]) : rest) =
          let pruneSolved = map (second (filter (/= solved))) rest
          in  (id, solved) : solve' (sortByConstraints pruneSolved)
      solved = solve' (sortByConstraints withColumn)
  in  map snd (sortOn fst solved)

main = do
  input <- readFile "input.txt"
  let (constraints, yourTicket, nearbyTickets) =
        case parse parseInput "" input of
          Left  e -> error (show e)
          Right r -> r
  print "Part 1:"
  print $ scanningErrorRate constraints nearbyTickets
  let ticketValues =
        rotateTickets (yourTicket : validTickets constraints nearbyTickets)
  print "Part 2:"
  let constraintOrder = solve (columnOptions constraints ticketValues)
  print $ product
    (map
      snd
      (filter (isPrefixOf "departure" . fst) (zip constraintOrder yourTicket))
    )
