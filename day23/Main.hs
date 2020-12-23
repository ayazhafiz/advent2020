import           Data.Array.ST
import           Data.List
import           Prelude                 hiding ( round )
import           Debug.Trace

splitOn :: Eq a => a -> [a] -> ([a], [a])
splitOn el lst =
  let walk :: Eq a => [a] -> a -> [a] -> ([a], [a])
      walk pre el (cur : post) | cur == el = (pre ++ [cur], post)
                               | otherwise = walk (pre ++ [cur]) el post
  in  walk [] el lst

move :: [Int] -> [Int]
move allCups@(currentCup : rest) =
  let
    (mn, mx) = (minimum allCups, maximum allCups)
    cups3    = take 3 rest
    allWout3 = currentCup : drop 3 rest
    label    = currentCup - 1
    findDestination :: Int -> Int
    findDestination label
      | label `elem` allCups && label `notElem` cups3
      = label
      | otherwise
      = let next = if label <= mn then mx else label - 1
        in  findDestination next
    dest        = findDestination label
    (pre, post) = splitOn dest allWout3
    fixed       = pre ++ cups3 ++ post
  in
    -- Cycle first cup to the back so next current cup is at the front.
    tail fixed ++ [head fixed]

moveN :: Int -> [Int] -> [Int]
moveN n lst = iterate move lst !! n

collectOrder :: [Int] -> String
collectOrder lst =
  let (b1, a1) = splitOn 1 lst in intercalate "" $ map show $ init $ a1 ++ b1

main = do
  let input = [1, 3, 5, 4, 6, 8, 7, 2, 9]

  print $ collectOrder $ moveN 100 input

  putStrLn "Part 1:"
  let input2 = take 1000000 $ input ++ [(length input + 1) ..]
  print $ take 15 $ moveN 1 input2

  putStrLn "Part 2:"
