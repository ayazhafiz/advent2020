import * as fs from 'fs';

const lines = fs.readFileSync('day9/input.txt').toString().split('\n');
console.error(`removing empty last line: ${lines.pop()}`);

const PLEN = 25;
const nums = lines.map(Number);

function is_valid(n: number, preamble: Map<number, number>): boolean {
  for (const cand of preamble.keys()) {
    if (preamble.has(n - cand) && cand !== n - cand) {
      return true;
    }
  }
  return false;
}

console.error(`Part 1:`);

const preamble = new Map<number, number>();
const addToPreamble = (n: number) => {
  if (preamble.has(n)) preamble.set(n, preamble.get(n) + 1);
  preamble.set(n, 1);
};
let i = 0;
for (; i < PLEN; ++i) {
  addToPreamble(nums[i]);
}
while (is_valid(nums[i], preamble)) {
  const n = nums[i];
  const nPLEN = nums[i - PLEN];
  preamble.set(nPLEN, preamble.get(nPLEN) - 1);
  if (preamble.get(nPLEN) === 0) preamble.delete(nPLEN);
  addToPreamble(n);
  ++i;
}

console.error(nums[i]);

console.error(`Part 2:`);

const target = nums[i];
outer: for (let i = 0;; ++i) {
  let sum = 0;
  for (let j = i;; ++j) {
    sum += nums[j];
    if (sum > target) {
      break;
    } else if (sum === target) {
      let max = nums[i], min = nums[i];
      for (let k = i; k <= j; ++k) {
        max = Math.max(max, nums[k]);
        min = Math.min(min, nums[k]);
      }
      console.error(max + min);
      break outer;
    }
  }
}
