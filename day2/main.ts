import * as fs from 'fs';

const RE_PW = /(.*?)-(.*?) (.): (.*)/;

console.error(`part 1:`);
const lines = fs.readFileSync('day2/input.txt').toString().split('\n');
{
  const valid = lines.map(l => RE_PW.exec(l)).filter((match) => {
    if (match === null) return false;
    const lo = Number(match[1]);
    const hi = Number(match[2]);
    const c = match[3];
    const pwd = match[4];
    const c_occur = pwd.split('').filter(ch => ch === c).length;
    return c_occur >= lo && c_occur <= hi;
  });
  console.log(valid.length);
}

console.error(`part 2:`);
{
  const valid = lines.map(l => RE_PW.exec(l)).filter((match) => {
    if (match === null) return false;
    const lo = Number(match[1]) - 1;
    const hi = Number(match[2]) - 1;
    const c = match[3];
    const pwd = match[4];
    if (pwd[lo] !== c) return pwd[hi] === c;
    if (pwd[hi] !== c) return pwd[lo] === c;
    return false;
  });
  console.log(valid.length);
}
