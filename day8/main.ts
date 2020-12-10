import * as fs from 'fs';

const lines = fs.readFileSync('day8/input.txt').toString().split('\n');
console.error(`removing empty last line: ${lines.pop()}`);

type Inst = {
  tag: 'acc'|'jmp'|'nop'; by: number;
}

const instructions: Inst[] = lines.map(line => {
  return {
    tag: line.substring(0, 3),
    by: Number(line.substring(3)),
  } as Inst;
});

console.error(instructions);

function exec(instructions: Inst[]): {success: boolean, counter: number} {
  const visited = new Set();
  let counter = 0;
  let curline = 0;
  while (!visited.has(curline) && curline !== instructions.length) {
    visited.add(curline);
    const inst = instructions[curline];
    switch (inst.tag) {
      case 'jmp':
        curline += inst.by;
        break;
      case 'acc':
        counter += inst.by;
        ++curline;
        break;
      case 'nop':
        ++curline;
        break;
    }
  }
  return {success: curline === instructions.length, counter};
}

{
  console.error(`Part 1:`);
  console.error(exec(instructions).counter);
}

{
  console.error(`Part 2:`);
  for (let i = 0; i < instructions.length; ++i) {
    const {tag, by} = instructions[i];
    if (tag === 'acc') continue;
    if (tag === 'jmp') {
      instructions[i] = {tag: 'nop', by};
    } else if (tag === 'nop') {
      instructions[i] = {tag: 'jmp', by};
    }

    const result = exec(instructions);
    if (result.success) {
      console.error(result.counter);
      break;
    }

    instructions[i] = {tag, by};
  }
}
