import Data.Set (Set)
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.String (Parser)

data Inst
  = Acc Int
  | Jmp Int
  | Nop Int

parseNumber :: Parser Int
parseNumber =
  let parseUnsigned :: Parser Int
      parseUnsigned = read <$> many1 digit
   in (char '-' >> (* (-1)) <$> parseUnsigned) <|> (char '+' >> parseUnsigned)

parseInst :: Parser Inst
parseInst =
  let parseRest :: Parser Int
      parseRest = do
        char ' '
        parseNumber
   in (string "acc" >> Acc <$> parseRest) <|>
      (string "jmp" >> Jmp <$> parseRest) <|>
      (string "nop" >> Nop <$> parseRest)

data EvalResult
  = FixedPoint Int Int
  | Terminated Int

counter :: EvalResult -> Int
counter (FixedPoint _ counter) = counter
counter (Terminated counter) = counter

eval :: [Inst] -> EvalResult
eval instructions =
  let lastInst = length instructions
   in let eval' :: Set Int -> Int -> Int -> EvalResult
          eval' seenInst counter curLine
            | curLine >= lastInst = Terminated counter
            | Set.member curLine seenInst = FixedPoint curLine counter
            | otherwise =
              let (counter', nextLine) =
                    case instructions !! curLine of
                      Acc by -> (counter + by, curLine + 1)
                      Jmp by -> (counter, curLine + by)
                      Nop _ -> (counter, curLine + 1)
               in eval' (Set.insert curLine seenInst) counter' nextLine
       in eval' Set.empty 0 0

data NoFixedPoint =
  NoFixedPoint

-- [ removeFixedPoint instructions ] returns a set of intructions with the first
-- discovered fixed point removed, or [ NoFixedPoint ] if no fixed point was
-- found.
removeFixedPoint :: [Inst] -> Either NoFixedPoint [Inst]
removeFixedPoint instructions =
  let tryCandidate :: [Inst] -> Inst -> [Inst] -> Maybe [Inst]
      tryCandidate before newI after =
        let cand = before ++ [newI] ++ after
         in case eval cand of
              FixedPoint _ _ -> Nothing
              Terminated _ -> Just cand
   in let recur :: [Inst] -> [Inst] -> Either NoFixedPoint [Inst]
          recur _ [] = Left NoFixedPoint
          recur prev (cur@(Acc _):next) = recur (prev ++ [cur]) next
          recur prev (cur@(Jmp by):next) =
            case tryCandidate prev (Nop by) next of
              Nothing -> recur (prev ++ [cur]) next
              Just solved -> Right solved
          recur prev (cur@(Nop by):next) =
            case tryCandidate prev (Jmp by) next of
              Nothing -> recur (prev ++ [cur]) next
              Just solved -> Right solved
       in recur [] instructions

main = do
  input <- lines <$> readFile "input.txt"
  let Right instructions = mapM (parse parseInst "") input
  print "Part 1:"
  print $ counter $ eval instructions
  print "Part 2:"
  let Right instructionsNoFp = removeFixedPoint instructions
  print $ counter $ eval instructionsNoFp
