import           Prelude                 hiding ( round )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

type Card = Int
type Deck = [Card]

data Winner = P1 | P2

parseCard :: Parser Card
parseCard = read <$> (many1 digit <* newline)

parsePlayer :: Parser Deck
parsePlayer = do
  string "Player "
  digit
  char ':'
  newline
  many1 parseCard

parse2Players :: Parser (Deck, Deck)
parse2Players = do
  one <- parsePlayer
  newline
  two <- parsePlayer
  return (one, two)

score :: Deck -> Int
score deck = sum $ zipWith (*) [1 ..] $ reverse deck

scoreWinner :: (Winner, Deck, Deck) -> Int
scoreWinner (P1, p1, _ ) = score p1
scoreWinner (P2, _ , p2) = score p2

draw :: Deck -> (Card, Deck)
draw []           = error "Cannot draw from an empty deck!"
draw (top : rest) = (top, rest)

collect :: (Card, Card) -> Deck -> Deck
collect (c1, c2) d = d ++ [c1, c2]

round :: (Deck, Deck) -> (Deck, Deck)
round (p1, p2) =
  let ((top1, rest1), (top2, rest2)) = (draw p1, draw p2)
      res | top1 > top2 = (collect (top1, top2) rest1, rest2)
          | top1 < top2 = (rest1, collect (top2, top1) rest2)
          | otherwise   = error "Cards are equal!"
  in  res

combat :: (Deck, Deck) -> (Winner, Deck, Deck)
combat (p1, []) = (P1, p1, [])
combat ([], p2) = (P2, [], p2)
combat decks    = combat $ round decks

roundR :: (Deck, Deck) -> (Deck, Deck)
roundR (p1, p2) =
  let ((top1, rest1), (top2, rest2)) = (draw p1, draw p2)
      win1                           = (collect (top1, top2) rest1, rest2)
      win2                           = (rest1, collect (top2, top1) rest2)
      res
        | top1 <= length rest1 && top2 <= length rest2
        = let rDeck1 = take top1 rest1
              rDeck2 = take top2 rest2
          in  case combatR (rDeck1, rDeck2) of
                (P1, _, _) -> win1
                (P2, _, _) -> win2
        | top1 > top2
        = win1
        | top1 < top2
        = win2
        | otherwise
        = error "Cards are equal!"
  in  res

combatR' :: Set (Deck, Deck) -> (Deck, Deck) -> (Winner, Deck, Deck)
combatR' _ (p1, []) = (P1, p1, [])
combatR' _ ([], p2) = (P2, [], p2)
combatR' seen decks@(d1, d2)
  | Set.member decks seen = (P1, d1, d2)
  | otherwise             = combatR' (Set.insert decks seen) (roundR decks)

combatR :: (Deck, Deck) -> (Winner, Deck, Deck)
combatR = combatR' Set.empty

main = do
  input <- readFile "input.txt"
  let Right (deck1, deck2) = parse parse2Players "" input

  putStrLn "Part 1:"
  print $ scoreWinner $ combat (deck1, deck2)

  putStrLn "Part 2:"
  print $ scoreWinner $ combatR (deck1, deck2)
