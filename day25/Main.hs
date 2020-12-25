ring = 20201227

invPower :: Int -> Int -> Int
invPower b word =
  let invPower' :: Int -> Int -> Int
      invPower' iterno word'
        | word' == word = iterno
        | otherwise     = invPower' (iterno + 1) ((word' * b) `rem` ring)
  in  invPower' 0 1

power :: Int -> Int -> Int
power b p =
  let power' :: Int -> Int -> Int
      power' 0 _    = 1
      power' 1 word = word
      power' n word = power' (n - 1) (word * b `rem` ring)
  in  power' p b

--               pkA    skB
diffieHellman :: Int -> Int -> Int
diffieHellman = power

assertEq :: (Eq a, Show a) => a -> a -> ()
assertEq a b = if a == b then () else error $ show a ++ " /= " ++ show b

main = do
  let b   = 7

  let pkA = 1614360
  let skA = invPower b pkA
  print skA
  let pkB = 7734663
  let skB = invPower b pkB
  print skB

  putStrLn "Part 1:"
  let secret  = diffieHellman pkA skB
  let secret2 = diffieHellman pkB skA
  let _       = assertEq secret secret2
  print secret
