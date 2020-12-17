{-# LANGUAGE FlexibleInstances #-}

import           Prelude                 hiding ( cycle )
import           Data.Bifunctor
import           Data.List               hiding ( cycle )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

data State
  = Active
  | Inactive

nextState :: Int -> State -> State
nextState 2 Active   = Active
nextState 3 Active   = Active
nextState _ Active   = Inactive
nextState 3 Inactive = Active
nextState _ Inactive = Inactive

toState :: Char -> State
toState '.' = Inactive
toState '#' = Active

instance Show State where
  show s = [toChar s]

toChar :: State -> Char
toChar Active   = '#'
toChar Inactive = '.'

isActive :: State -> Bool
isActive Active = True
isActive _      = False

type Space a = Map a State
type Position2 = (Int, Int)
type Position3 = (Int, Int, Int)
type Position4 = (Int, Int, Int, Int)

parseFloor :: String -> Space Position2
parseFloor floor = Map.fromList $ concat $ zipWith
  (\r row -> zipWith (\c cube -> ((c, r), toState cube)) [0 ..] row)
  [0 ..]
  (lines floor)

class (Ord a) => Position a where
  from2 :: Position2 -> a
  into2 :: a -> Position2
  neighbors :: a -> [a]

instance Position Position3 where
  from2 (x, y) = (x, y, 0)
  into2 (x, y, _) = (x, y)
  neighbors (x, y, z) =
    [ (nx, ny, nz)
    | nx <- [x - 1 .. x + 1]
    , ny <- [y - 1 .. y + 1]
    , nz <- [z - 1 .. z + 1]
    , (nx, ny, nz) /= (x, y, z)
    ]

instance Position Position4 where
  from2 (x, y) = (x, y, 0, 0)
  into2 (x, y, _, _) = (x, y)
  neighbors (x, y, z, w) =
    [ (nx, ny, nz, nw)
    | nx <- [x - 1 .. x + 1]
    , ny <- [y - 1 .. y + 1]
    , nz <- [z - 1 .. z + 1]
    , nw <- [w - 1 .. w + 1]
    , (nx, ny, nz, nw) /= (x, y, z, w)
    ]

step :: (Position a) => a -> Space a -> State
step pos space =
  let neighborStates =
          map (\p -> Map.findWithDefault Inactive p space) (neighbors pos)
      activeNeighbors = length $ filter isActive neighborStates
  in  nextState activeNeighbors (Map.findWithDefault Inactive pos space)

cycle :: (Position a) => Space a -> Space a
cycle space =
  -- The only points that might now be active are the neighbors of the space we
  -- are presently cycling over.
  let nextPoints = Map.keys space >>= neighbors
  in  Map.fromList $ map (\pos -> (pos, step pos space)) nextPoints

cycleN :: (Position a) => Int -> Space a -> Space a
cycleN n space = iterate cycle space !! n

numActive :: Space a -> Int
numActive space = length $ filter isActive $ Map.elems space

showLayer :: [(Position2, State)] -> String
showLayer space =
  let rows = groupBy (\((_, y1), _) ((_, y2), _) -> y1 == y2)
                     (sortOn (snd . fst) space)
      sortx = sortOn (fst . fst)
  in  unlines $ map (map (toChar . snd) . sortx) rows

showLayers :: Space Position3 -> String
showLayers space =
  let sortedZ = sortOn (\((_, _, z), _) -> z) (Map.toList space)
      layersZ = groupBy (\((_, _, z1), _) ((_, _, z2), _) -> z1 == z2) sortedZ
      lower   = map (first into2)
  in  unlines $ map (showLayer . lower) layersZ

main = do
  input <- readFile "input.txt"
  let space2 = parseFloor input

  print "Part 1:"
  let space3 = Map.mapKeys (from2 :: Position2 -> Position3) space2
  print $ numActive $ cycleN 6 space3

  print "Part 2:"
  let space4 = Map.mapKeys (from2 :: Position2 -> Position4) space2
  print $ numActive $ cycleN 6 space4
