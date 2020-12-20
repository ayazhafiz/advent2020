{-# LANGUAGE LambdaCase #-}

import           Data.List
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

parseTileLine :: Parser String
parseTileLine = do
  line <- many1 (char '#' <|> char '.')
  newline
  return line

parseTile :: Parser (Int, [String])
parseTile = do
  string "Tile "
  no <- read <$> many1 digit
  char ':'
  newline
  tile <- many1 parseTileLine
  newline
  return (no, tile)

parseTiles :: Parser [(Int, [String])]
parseTiles = many1 parseTile

-- Given a tile, returns its edges in all possible orientations.
-- (top, bottom, left, right, each of which can be reversed as well).
tileEdges :: [String] -> [String]
tileEdges tile =
  let top    = head tile
      bottom = last tile
      left   = map head tile
      right  = map last tile
  in  [top, bottom, left, right] >>= \x -> [x, reverse x]

-- Records a tile ID as having a given edge in a map.
recordTileEdge edge tid = Map.alter
  (Just . \case
    Nothing -> Set.singleton tid
    Just s  -> Set.insert tid s
  )
  edge

-- Records a tile ID as associated with a given set of edges in a map.
recordTileEdges edges tid mp =
  foldl' (\m edge -> recordTileEdge edge tid m) mp edges

borderEdges = Map.filter (\m -> Set.size m == 1)

corners edge2tis =
  map head
    -- 2 sides not matching any others * 2 permutations each
    $   filter (\m -> length m == 4)
    $   group
    $   sort
    $   Map.elems (borderEdges edge2tis)
    >>= Set.toList

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 go c b a = go a b c

main = do
  input <- readFile "input.txt"
  let Right tiles = parse parseTiles "" input

  let ti2edges    = Map.map tileEdges $ Map.fromList tiles
  let edge2tis = Map.foldlWithKey' (flip3 recordTileEdges) Map.empty ti2edges

  putStrLn "Part 1:"
  let cornerTIds = corners edge2tis
  print $ product cornerTIds

  putStrLn "Part 2:"
