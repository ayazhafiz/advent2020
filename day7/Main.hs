import           Text.Parsec
import           Text.Parsec.String             ( Parser )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

type Bag = String
type Bag2Contents = Map Bag (Map Bag Int)
type Bag2ContainedBy = Map Bag (Set Bag)

parseBag :: Parser Bag
parseBag = do
  adj <- many1 letter
  char ' '
  color <- many1 letter
  string " bag"
  optional (char 's')
  return (adj ++ " " ++ color)

parseBagCount :: Parser (Bag, Int)
parseBagCount = do
  count <- read <$> many1 digit
  char ' '
  bag <- parseBag
  return (bag, count)

parseBagInfo :: Parser (Bag, Map Bag Int)
parseBagInfo = do
  bag <- parseBag
  string " contain "
  contents <-
    Map.fromList
      <$> (   (string "no other bags" >> return [])
          <|> sepBy1 parseBagCount (string ", ")
          )
  char '.'
  return (bag, contents)

main = do
  input <- lines <$> readFile "input.txt"
  let bag2contents =
        let Right bag2contents' = mapM (parse parseBagInfo "") input
        in  Map.fromList bag2contents'

  let bag2containedBy =
        (let invert
               :: [(Bag, Map Bag Int)] -> Bag2ContainedBy -> Bag2ContainedBy
             invert [] known = known
             invert ((outer, contents) : rest) known =
               let known' = foldl
                     (\mp innerBag ->
                       let containedBy =
                             Map.findWithDefault Set.empty innerBag mp
                       in  let containedBy' = Set.insert outer containedBy
                           in  Map.insert innerBag containedBy' mp
                     )
                     known
                     (Map.keys contents)
               in  invert rest known'
         in  invert (Map.toList bag2contents) Map.empty
        )

  print "Part 1:"
  let numContainingShinyGold =
        (let
           recur :: Set Bag -> [Bag] -> Int
           recur contain [] = Set.size contain
           recur contain (cur : rest) =
             let
               curContainedBy =
                 Set.toList $ Map.findWithDefault Set.empty cur bag2containedBy
             in  let contain' = foldl (flip Set.insert) contain curContainedBy
                 in  recur contain' (rest ++ curContainedBy)
         in
           recur Set.empty ["shiny gold"]
        )
  print numContainingShinyGold

  print "Part 2:"
  let numBagsWithinShinyGold =
        (let recur :: Bag -> Int
             recur bag = 1 + sum
               (map (\(bag, count) -> count * recur bag)
                    (Map.toList $ Map.findWithDefault Map.empty bag bag2contents)
               )
         in  recur "shiny gold" - 1
        )
  print numBagsWithinShinyGold
