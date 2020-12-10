import * as fs from 'fs';

const lines = fs.readFileSync('day3/input.txt').toString().split('\n');
lines.pop();  // trailing newline

function trees_on_path(slope: {right: number, down: number}): number {
  const LINELEN = lines[0].length;
  let y = 0, x = 0;
  let trees = 0;
  while (y < lines.length) {
    if (lines[y][x] === '#') ++trees;
    y += slope.down;
    x = (x + slope.right) % LINELEN;
  }
  return trees;
}

{
  console.error(`part 1:`);
  console.log(trees_on_path({right: 3, down: 1}));
}

{
  console.error(`part 2:`);
  const slopes = [
    {right: 1, down: 1},
    {right: 3, down: 1},
    {right: 5, down: 1},
    {right: 7, down: 1},
    {right: 1, down: 2},
  ];
  let res = 1;
  for (const slope of slopes) res *= trees_on_path(slope);
  console.log(res);
}
