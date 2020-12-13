import           Data.List
import           Data.Maybe
import           Data.Ord
import           Text.Parsec             hiding ( Empty )
import           Text.Parsec.String             ( Parser )

type Bus = Maybe Int

parseBus :: Parser Bus
parseBus =
  let parseUnsigned :: Parser Int
      parseUnsigned = read <$> many1 digit
  in  (string "x" >> return Nothing) <|> (Just <$> parseUnsigned)

parseBuses :: Parser [Bus]
parseBuses = parseBus `sepBy` char ','

part1 :: Int -> [Bus] -> Int
part1 departTime buses =
  let waitTimes = map
        (\interval ->
          let wait = ((-departTime) `mod` interval) in (wait, interval)
        )
        (catMaybes buses)
      (wait, firstBus) = minimumBy (comparing fst) waitTimes
  in  wait * firstBus

multInv :: Int -> Int -> Int
multInv _ 1 = 1
multInv n ring =
  let recur n ring ring0 x0 x1
        | n > 1
        = let q            = n `div` ring
              (a' , ring') = (ring, n `mod` ring)
              (x0', x1'  ) = (x1 - q * x0, x0)
          in  recur a' ring' ring0 x0' x1'
        | x1 < 0
        = x1 + ring0
        | otherwise
        = x1
  in  recur n ring ring 0 1

chineseRemainder :: [Int] -> [Int] -> Int
chineseRemainder n a =
  let
    prod = product n
    sumAll =
      sum
        (zipWith (\ni ai -> let p = (prod `div` ni) in ai * multInv p ni * p)
                 n
                 a
        )
  in
    sumAll `mod` prod

part2 :: [Bus] -> Int
part2 buses =
  -- build up a set of simultaneous equations
  --   departTime ~= a_1 mod n_1
  --   departTime ~= a_2 mod n_2
  --   ...
  let an_s = catMaybes
        (zipWith (\bus wait -> fmap (\iv -> (iv - wait, iv)) bus) buses [0 ..])
      as = map fst an_s
      ns = map snd an_s
  in  chineseRemainder ns as

main = do
  input <- lines <$> readFile "input.txt"
  let departTime  = read $ head input :: Int
  let Right buses = parse parseBuses "" (last input)
  print "Part 1:"
  print $ part1 departTime buses
  print "Part 2:"
  print $ part2 buses
