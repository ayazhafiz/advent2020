import * as fs from 'fs';

console.error(`Part 1:`);
const nums =
    fs.readFileSync('day1/input.txt').toString().split('\n').map(Number)
const seen = new Set();
outer: for (const num of nums) {
  if (seen.has(2020 - num)) {
    console.log(num * (2020 - num));
    break outer;
  }
  seen.add(num);
}

console.error(`Part 2:`);
const allNums = new Set(nums);
outer: for (const m of allNums) {
  for (const n of allNums) {
    if (allNums.has(2020 - m - n)) {
      console.log(m * n * (2020 - m - n));
      break outer;
    }
  }
}
