import * as fs from 'fs';

const lines = fs.readFileSync('day7/input.txt').toString().split('\n');
console.error(`removing empty last line: ${lines.pop()}`);

const bag2content: Map<string, Record<string, number>> =
    new Map(lines.map(line => {
      const [bag, contains] = line.split(' bags contain ');
      const content = {};
      if (contains !== 'no other bags.') {
        for (const dependency of contains.split(',')) {
          const match = /(\d+) (.*?) bags?/.exec(dependency);
          content[match[2]] = Number(match[1]);
        }
      }
      return [bag, content];
    }));

const bag2containing: Map<string, Set<string>> = new Map();
for (const [outerBag, contains] of bag2content) {
  for (const innerBag of Object.keys(contains)) {
    if (!bag2containing.has(innerBag)) {
      bag2containing.set(innerBag, new Set());
    }
    bag2containing.get(innerBag).add(outerBag);
  }
}

{
  console.error(`Part 1:`);
  const canContainShinyGold = new Set();
  let toVisit = new Set(['shiny gold']);
  while (toVisit.size > 0) {
    const next: Set<string> = new Set();
    for (const innerBag of toVisit) {
      const outerBags = bag2containing.get(innerBag);
      if (!outerBags) continue;
      for (const outerBag of outerBags) {
        if (bag2content.get(outerBag)![innerBag] >= 1) {
          canContainShinyGold.add(outerBag);
          next.add(outerBag);
        }
      }
    }

    toVisit = next;
  }
  console.error(canContainShinyGold.size);
}

function numBagsWithin(bag: string): number {
  let total = 1;  // self
  for (const [innerBag, num] of Object.entries(bag2content.get(bag)!)) {
    total += num * numBagsWithin(innerBag);
  }
  return total;
}

{
  console.error(`Part 2:`);
  console.error(numBagsWithin('shiny gold') - 1);
}
