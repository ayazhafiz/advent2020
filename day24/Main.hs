{-# LANGUAGE LambdaCase #-}

import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

data Direction
  = E
  | SE
  | SW
  | W
  | NW
  | NE

parseDirection :: Parser Direction
parseDirection =
  try (string "e" >> return E)
    <|> try (string "se" >> return SE)
    <|> try (string "sw" >> return SW)
    <|> try (string "w" >> return W)
    <|> try (string "nw" >> return NW)
    <|> (string "ne" >> return NE)

parseSteps :: Parser [Direction]
parseSteps = many1 parseDirection

-- https://www.redblobgames.com/grids/hexagons/
-- Store a position on a hexagonal grid as an (x, y, z) coordinate tuple.
-- The idea is to represent the grid on 3 axes. Consider "o" to be the reference
-- marker. Then we have
--
--                      v------- "z" axis
--                      |   v--- "x" axis
--                [+y][-z][+x]
--                   \  |  /
--  NW       (0, 1, -1) | (1, 0, -1)       NE
--                     \|/
--   W  (-1, 1, 0)  (0, 0, 0)  (1, -1, 0)  E
--                     /|\
--  SW       (-1, 0, 1) | (0, -1, 1)       SE
--                   /  |  \
--                [-y][+z][-x]
--                          ^--- "y" axis
--
-- Notice that the coordinate movements may seem to "shift" move than they need
-- to; for example, the NW of (0, 0, 0) is (1, 0, -1), where the z axis shifts
-- as well as the x axis. This preserves the invariant x + y + z = 0, and also
-- means the hexagonal grid is a strict subset of Z3. 
type HexP = (Int, Int, Int)

neighbor :: HexP -> Direction -> HexP
neighbor (x, y, z) E  = (x + 1, y - 1, z)
neighbor (x, y, z) SE = (x, y - 1, z + 1)
neighbor (x, y, z) SW = (x - 1, y, z + 1)
neighbor (x, y, z) W  = (x - 1, y + 1, z)
neighbor (x, y, z) NW = (x, y + 1, z - 1)
neighbor (x, y, z) NE = (x + 1, y, z - 1)

neighbors :: HexP -> [HexP]
neighbors tile = map (neighbor tile) [E, SE, SW, W, NW, NE]

selfAndNeighbors :: HexP -> [HexP]
selfAndNeighbors tile = tile : neighbors tile

-- 0 for White, 1 for Black
flipTile :: Map HexP Int -> HexP -> Map HexP Int
flipTile floor tile = Map.alter
  (Just . \case
    -- Initially white, flip to black
    Nothing -> 1
    Just 0  -> 1
    Just 1  -> 0
  )
  tile
  floor

uniq :: Ord a => [a] -> [a]
uniq = Set.toList . Set.fromList

getColor :: Map HexP Int -> HexP -> Int
getColor floor tile = Map.findWithDefault 0 tile floor

updateTile :: HexP -> Map HexP Int -> Int
updateTile tile floor =
  let nBlackTiles = sum $ map (getColor floor) (neighbors tile)
  in 
      -- Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
      -- Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
      case getColor floor tile of
        1 -> if nBlackTiles == 0 || nBlackTiles > 2 then 0 else 1
        0 -> if nBlackTiles == 2 then 1 else 0

updateFloor :: Map HexP Int -> Map HexP Int
updateFloor floor =
  -- We need to update all tiles we know about, and also all of *their* neighbors.
  let toUpdate = uniq $ Map.keys floor >>= selfAndNeighbors
  in  Map.fromList $ map (\tile -> (tile, updateTile tile floor)) toUpdate

daysN :: Int -> Map HexP Int -> Map HexP Int
daysN n floor = iterate updateFloor floor !! n

countBlackTiles :: Map HexP Int -> Int
countBlackTiles = length . Map.filter (== 1)

main = do
  input <- lines <$> readFile "input.txt"
  let Right tileSteps = mapM (parse parseSteps "") input
  let refTile         = (0, 0, 0)

  putStrLn "Part 1:"
  let tilesToFlip = map (foldl' neighbor refTile) tileSteps
  let floor       = foldl' flipTile Map.empty tilesToFlip
  print $ countBlackTiles floor

  putStrLn "Part 2:"
  print $ countBlackTiles $ daysN 100 floor
