import           Data.List                      ( sort )
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map

main = do
  input <- lines <$> readFile "input.txt"
  let (adapters, max_adapter) =
        (let nums'       = sort $ map read input :: [Int]
             max_adapter = maximum nums' + 3
         in  (nums' ++ [max_adapter], max_adapter)
        )

  print "Part 1:"
  let (diff1, diff3, _) =
        (let getDiffs :: (Int, Int, Int) -> Int -> (Int, Int, Int)
             getDiffs (diff1, diff3, last) cur = case cur - last of
               1 -> (diff1 + 1, diff3, cur)
               3 -> (diff1, diff3 + 1, cur)
               _ -> error "Bad state: diff not 1 or 3"
         in  foldl getDiffs (0, 0, 0) adapters
        )
  print (diff1 * diff3)

  print "Part 2:"
  let known_adapters = Set.fromAscList adapters
  let
    maxAdapterConnectWays =
      (let
         updateConnections :: Map.Map Int Int -> Int -> Map.Map Int Int
         updateConnections mpConnections adapter =
           if not (Set.member adapter known_adapters)
             then mpConnections
             else
               let adapter_connect_ways = sum $ map
                     (\n -> Map.findWithDefault 0 n mpConnections)
                     [adapter - 3 .. adapter - 1]
               in  Map.insert adapter adapter_connect_ways mpConnections
         connectWays =
           foldl updateConnections (Map.singleton 0 1) [1 .. max_adapter]
       in
         Map.findWithDefault 0 max_adapter connectWays
      )
  print maxAdapterConnectWays
