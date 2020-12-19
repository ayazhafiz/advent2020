import           Data.Either                    ( isRight )
import           Data.List
import           Control.Monad                  ( void )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

data Rule
  = Exact String
  | Seq [Int]
  | Or Rule Rule

instance Show Rule where
  show (Exact s    ) = "\"" ++ s ++ "\""
  show (Seq   rules) = unwords $ map show rules
  show (Or r1 r2   ) = show r1 ++ " | " ++ show r2

showRule :: (Int, Rule) -> String
showRule (n, r) = show n ++ ": " ++ show r

parseUnsigned :: Parser Int
parseUnsigned = read <$> many1 digit

parseExact :: Parser Rule
parseExact = do
  char '"'
  s <- many1 letter
  char '"'
  return $ Exact s

parseSeq :: Parser Rule
parseSeq = Seq <$> many1 (parseUnsigned <* optional (char ' '))

parseOr :: Parser Rule
parseOr = do
  r1 <- parseSeq
  string "| "
  Or r1 <$> parseSeq

parseRule :: Parser Rule
parseRule = parseExact <|> try parseOr <|> parseSeq

parseNamedRule :: Parser (Int, Rule)
parseNamedRule = do
  no <- parseUnsigned
  string ": "
  rule <- parseRule
  return (no, rule)

parseMessage :: Parser String
parseMessage = many1 letter

withLf :: Parser a -> Parser a
withLf p = do
  res <- p
  newline
  return res

parseInput :: Parser ([(Int, Rule)], [String])
parseInput = do
  rules <- many1 (withLf parseNamedRule)
  newline
  messages <- many1 (withLf parseMessage)
  return (rules, messages)

getR :: Int -> Map Int Rule -> Rule
getR ruleNo rules = let Just rule = Map.lookup ruleNo rules in rule

match :: Rule -> Map Int Rule -> Parser ()
match (Exact s) _ = void $ string s
match (Seq ruleNos) rules =
  let matchSeq :: [Int] -> Parser ()
      matchSeq []           = return ()
      matchSeq (cur : rest) = do
        match (getR cur rules) rules
        matchSeq rest
  in  matchSeq ruleNos
match (Or r1 r2) rules = try (match r1 rules) <|> match r2 rules

matchTop :: Int -> Map Int Rule -> Parser ()
matchTop ruleNo rules = do
  match (getR ruleNo rules) rules
  eof

main = do
  input <- readFile "input.txt"
  let Right (sRules, messages) = parse parseInput "" input
  let rules                    = Map.fromList sRules
  putStrLn "Part 1:"
  let match0 ruleMap = parse (matchTop 0 ruleMap) ""
  let numMatch0 ruleMap =
        length $ filter isRight (map (match0 ruleMap) messages)
  print $ numMatch0 rules
  putStrLn "Part 2:"
  let Right part2adjust = mapM (parse parseNamedRule "")
                               ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]
  let rules2 = foldl' (\rules (ruleNo, rule) -> Map.insert ruleNo rule rules)
                      rules
                      part2adjust
  print $ numMatch0 rules2
