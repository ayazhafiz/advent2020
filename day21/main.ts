import * as fs from 'fs';

/** Type `Nomimal<T, Brand>` is a subtype of `T` and is covariant on `Brand`. */
type Nominal<T, brand> = T&{__brand: brand};

type Ingredient = Nominal<string, 'ingredient'>;
type Allergen = Nominal<string, 'allergen'>;

interface IngredientList {
  ingredients: Ingredient[];
  allergens: Allergen[];
}

function parseIngredientList(list: string): IngredientList {
  const [igs, ags] = list.split(' (contains ');
  return {
    ingredients: igs.split(' ').map(s => s as Ingredient),
    allergens: ags.slice(0, /* ')' */ -1).split(', ').map(s => s as Allergen),
  };
}

function possibleAllergens(ingredientLists: IngredientList[]):
    Map<Allergen, Set<Ingredient>> {
  const constraints = new Map<Allergen, Set<Ingredient>>();
  for (const {ingredients, allergens} of ingredientLists) {
    for (const allergen of allergens) {
      const curIgs = new Set(ingredients);

      if (!constraints.has(allergen)) {
        constraints.set(allergen, curIgs);
      }

      const allergenIgs = constraints.get(allergen)!;
      for (const ig of allergenIgs) {
        if (!curIgs.has(ig)) {
          allergenIgs.delete(ig);
        }
      }
    }
  }
  return constraints;
}

function solveContraints<S, T>(constraints: Map<S, Set<T>>): Map<S, T> {
  const allSolved = () => [...constraints.values()].every(c => c.size === 1);
  const solvedConstraints = new Map<S, T>();
  while (!allSolved()) {
    for (const [toSolve, opts] of constraints) {
      if (!solvedConstraints.has(toSolve) && opts.size === 1) {
        const [solved] = [...opts.values()];
        for (const [other, otherOpts] of constraints) {
          if (other !== toSolve) {
            otherOpts.delete(solved);
          }
        }
        solvedConstraints.set(toSolve, solved);
      }
    }
  }
  return solvedConstraints;
}

function main() {
  const lines = fs.readFileSync('day21/input.txt').toString().split('\n');
  lines.pop();

  const ingredientLists = lines.map(parseIngredientList);
  const allergenConstraints = possibleAllergens(ingredientLists);
  const solved = solveContraints(allergenConstraints);

  const part2 = [...solved.entries()]
                    .sort(([a1, _], [a2, __]) => {
                      if (a1 < a2) {
                        return -1;
                      } else {
                        return 1;
                      }
                    })
                    .map(([_, i]) => i);
  console.log(part2.join(','));
}

main();
