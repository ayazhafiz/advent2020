import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map

stepGame :: (Int, Int, Map Int Int) -> (Int, Map Int Int)
stepGame (turnno, cur, state) =
  let state' = Map.insert cur turnno state
  in  if Map.notMember cur state
        then (0, state')
        else (turnno - Map.findWithDefault 0 cur state, state')

doGame :: Int -> Int -> Map Int Int -> Int
doGame 30000000 cur _ = cur
doGame turnno cur state =
  let (next, state') = stepGame (turnno, cur, state)
  in  doGame (turnno + 1) next state'

main = do
  let input = [0, 13, 1, 16, 6, 17]
  let num2turn =
        foldl' (\map (i, n) -> Map.insert n i map) Map.empty (zip [1 ..] input)
  print "Part 1:"
  print $ doGame (length input) (last input) num2turn
  print "Part 2:"
