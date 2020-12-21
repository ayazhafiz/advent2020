{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

import           Data.Bifunctor
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Text.Parsec
import           Text.Parsec.String             ( Parser )

type Ingredient = String
type Allergen = String

parseWord :: Parser String
parseWord = many1 lower

parseIngredientList :: Parser ([Ingredient], [Allergen])
parseIngredientList = do
  ingredients <- many1 (parseWord <* space)
  string "(contains "
  allergens <- parseWord `sepBy` string ", "
  return (ingredients, allergens)

type AConstraints = Map Allergen (Set Ingredient)

addConstraint :: Allergen -> [Ingredient] -> AConstraints -> AConstraints
addConstraint allergen ingredients =
  let newIngredients = Set.fromList ingredients
  in  Map.alter
        (Just . \case
          Nothing       -> newIngredients
          Just existing -> Set.intersection existing newIngredients
        )
        allergen

addILConstraints :: ([Ingredient], [Allergen]) -> AConstraints -> AConstraints
addILConstraints il constraints =
  let (igs, allergens) = il
  in  foldl' (\c allergen -> addConstraint allergen igs c) constraints allergens

solveConstraints :: AConstraints -> Map Allergen Ingredient
solveConstraints constraints =
  let constr     = Map.toList constraints
      sortConstr = sortOn (Set.size . snd)
      walk :: [(Allergen, Set Ingredient)] -> [(Allergen, Ingredient)]
      walk [] = []
      walk ((allergen, Set.elems -> [ingredient]) : rest) =
          let rest' = sortConstr $ map (second $ Set.delete ingredient) rest
          in  (allergen, ingredient) : walk rest'
      walk _ = error "Constraints not uniquely solvable!"
  in  Map.fromList $ walk $ sortConstr constr

ingredients :: [([Ingredient], [Allergen])] -> Set Ingredient
ingredients = foldl' (\s (igs, _) -> Set.union s (Set.fromList igs)) Set.empty

appearancesAny :: [([Ingredient], [Allergen])] -> Set Ingredient -> Int
appearancesAny ils cands = foldl'
  (\count (igs, _) -> count + length (filter (`Set.member` cands) igs))
  0
  ils

invert :: (Ord a, Ord b) => Map a b -> Map b a
invert mp = Map.fromList $ map (\(a, b) -> (b, a)) $ Map.toList mp

mGet :: Ord a => a -> Map a b -> b
mGet a mp = case Map.lookup a mp of
  Just b -> b

main = do
  input <- lines <$> readFile "input.txt"
  let Right ingredientLists = mapM (parse parseIngredientList "") input

  let allIngredients        = ingredients ingredientLists
  let constraints = foldl' (flip addILConstraints) Map.empty ingredientLists
  let allergens             = solveConstraints constraints
  let ingWAllergens         = Set.fromList $ map snd (Map.toList allergens)

  putStrLn "Part 1:"
  let ingWOutAllergens = Set.difference allIngredients ingWAllergens
  print $ appearancesAny ingredientLists ingWOutAllergens

  putStrLn "Part 2:"
  let ing2Allergen = invert allergens
  putStrLn $ intercalate "," $ sortOn (`mGet` ing2Allergen)
                                      (Set.toList ingWAllergens)
