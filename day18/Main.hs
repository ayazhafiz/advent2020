import           Text.Parsec
import           Text.Parsec.String             ( Parser )

data Tree
  = Plus Tree Tree
  | Times Tree Tree
  | Num Int
  deriving(Show)

parseUnsigned :: Parser Int
parseUnsigned = read <$> many1 digit

parseOp :: Parser (Tree -> Tree -> Tree)
parseOp = (char '+' >> return Plus) <|> (char '*' >> return Times)

parseParenthesized :: Parser Tree -> Parser Tree
parseParenthesized parseExpr = do
  char '('
  expr <- parseExpr
  char ')'
  return expr

parseExpression1 :: Parser Tree
parseExpression1 = do
  let parseRest tree = buildTree tree <|> return tree
      parseTerm =
        parseParenthesized parseExpression1 <|> (Num <$> parseUnsigned)
      buildTree l =
        (do
          char ' '
          op <- parseOp
          char ' '
          r <- parseTerm
          parseRest (op l r)
        )
  parseTerm >>= parseRest

parseExpression2 :: Parser Tree
parseExpression2 = do
  let parseRest builder tree = try (builder tree) <|> return tree
      parseTopLevel =
        parseParenthesized parseExpression2 <|> (Num <$> parseUnsigned)
      parsePlus =
        (do
          let build l =
                (do
                  string " + "
                  r <- parseTopLevel
                  parseRest build (Plus l r)
                )
          parseTopLevel >>= parseRest build
        )
      parseTimes =
        (do
          let build l =
                (do
                  string " * "
                  r <- parsePlus
                  parseRest build (Times l r)
                )
          parsePlus >>= parseRest build
        )
  parseTimes

eval :: Tree -> Int
eval (Plus  n m) = eval n + eval m
eval (Times n m) = eval n * eval m
eval (Num n    ) = n

main = do
  input <- lines <$> readFile "input.txt"

  print "Part 1:"
  let Right expressions1 = mapM (parse parseExpression1 "") input
  print $ sum $ map eval expressions1

  print "Part 2:"
  let Right expressions2 = mapM (parse parseExpression2 "") input
  print $ sum $ map eval expressions2
