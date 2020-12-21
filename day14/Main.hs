{-# LANGUAGE FlexibleInstances #-}

import           Data.Bits
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Text.Parsec             hiding ( Empty )
import           Text.Parsec.String             ( Parser )

data Command
  --     or  and
  = Mask String
  --      addr value
  | Write Int Int

emptyMask = (2 ^ 36) - 1

showMask :: Int -> String
showMask n =
  let recur :: Int -> Int -> String
      recur 36 _ = ""
      recur bitno bit =
          recur (bitno + 1) (bit * 2) ++ show (fromEnum $ (bit .&. n) == bit)
  in  recur 0 1

parseUnsigned :: Parser Int
parseUnsigned = read <$> many1 digit

parseMask :: Parser Command
parseMask = do
  string "mask = "
  Mask <$> many1 (char 'X' <|> char '0' <|> char '1')

parseWrite :: Parser Command
parseWrite = do
  string "mem["
  addr <- parseUnsigned
  string "] = "
  Write addr <$> parseUnsigned

parseCommand :: Parser Command
parseCommand = do
  try parseMask <|> parseWrite

type Memory = Map Int Int

class MemoryState a where
  memory :: a -> Memory

--                        or   and  masks
type MemState1 = (Memory, Int, Int)
instance MemoryState MemState1 where
  memory (m, _, _) = m

parseMask1 :: String -> (Int, Int)
-- X10X0 => (or 01000 . and 11010)
-- -  -     these should just observe the original value
--  -- -    these should be forced to the new value
-- Thus, pull out the "1"s into the "or" mask and the "0"s into the "and" mask.
-- Everything else is "0"s in the "or" mask, "1"s in the "and" mask.
-- or  := 01000
-- and := 11010
parseMask1 s =
  let (or, and, _) = foldr
        (\l (or, and, i) -> case l of
          'X' -> (or `xor` i, and .|. i, i * 2)
          '0' -> (or `xor` i, and `xor` i, i * 2)
          '1' -> (or .|. i, and .|. i, i * 2)
        )
        (emptyMask, emptyMask, 1)
        s
  in  (or, and)

applyCommand1 :: MemState1 -> Command -> MemState1
applyCommand1 (memory, _, _) (Mask s) =
  let (or, and) = parseMask1 s in (memory, or, and)
applyCommand1 (memory, or, and) (Write addr value) =
  let memory' = Map.insert addr ((value .|. or) .&. and) memory
  in  (memory', or, and)

permuteAddrs :: String -> Int -> [Int]
-- X10X0 ~> 10101 => X11X1 => [01101, 01111, 11101, 11111]
-- -  -     these should permute to any combination of 0/1
--  -- -    these should be "or"ed with the original values
permuteAddrs s base =
  let (permutations, _) = foldr
        (\l (permutes, i) ->
          let permutes' = case l of
                'X' -> permutes >>= (\n -> [n .|. i, n .&. complement i])
                '0' -> permutes
                '1' -> map (.|. i) permutes
          in  (permutes', i * 2)
        )
        ([base], 1)
        s
  in  permutations

type MemState2 = (Map Int Int, String)
instance MemoryState MemState2 where
  memory (m, _) = m

applyCommand2 :: MemState2 -> Command -> MemState2
applyCommand2 (memory, _) (Mask s) = (memory, s)
applyCommand2 (memory, mask) (Write addr value) =
  let writeto = permuteAddrs mask addr
      memory' = foldl (\map a -> Map.insert a value map) memory writeto
  in  (memory', mask)

emulate :: MemoryState s => (s -> Command -> s) -> s -> [Command] -> Int
emulate folder initState commands =
  let result = foldl' folder initState commands
  in  sum $ Map.elems $ memory result

main = do
  input <- lines <$> readFile "input.txt"
  let Right cmds = mapM (parse parseCommand "") input
  print "Part 1:"
  print $ emulate applyCommand1 (Map.empty, emptyMask, emptyMask) cmds
  print "Part 2:"
  print $ emulate applyCommand2 (Map.empty, "") cmds
