import * as fs from 'fs';

const lines = fs.readFileSync('day12/input.txt').toString().split('\n');
console.error(`removing empty last line: ${lines.pop()}`);

interface Cmd {
  cmd: 'N'|'S'|'E'|'W'|'L'|'R'|'F';
  by: number;
}

function route(cmds: Cmd[]) {
  const pos = {x: 0, y: 0};
  const dirs = [{x: 1, y: 0}, {x: 0, y: -1}, {x: -1, y: 0}, {x: 0, y: 1}];
  let curDir = 0;
  for (const {cmd, by} of cmds) {
    switch (cmd) {
      case 'N': {
        pos.y += by;
        break;
      }
      case 'S': {
        pos.y -= by;
        break;
      }
      case 'W': {
        pos.x -= by;
        break;
      }
      case 'E': {
        pos.x += by;
        break;
      }
      case 'L': {
        const minus = by / 90;
        curDir = (((curDir - minus) % 4) + 4) % 4;
        break;
      }
      case 'R': {
        const plus = by / 90;
        curDir = (curDir + plus) % 4;
        break;
      }
      case 'F': {
        pos.x += dirs[curDir].x * by;
        pos.y += dirs[curDir].y * by;
        break;
      }
    }
  }
  return pos;
}

const dirs: Cmd[] = lines.map(line => {
  return {
    cmd: line[0],
    by: Number(line.substring(1)),
  } as Cmd;
});

console.error(`Part 1:`);
{
  const {x, y} = route(dirs);
  console.log(Math.abs(x) + Math.abs(y));
}

function route2(cmds: Cmd[]) {
  const pos = {x: 0, y: 0};
  const waypoint = {x: 10, y: 1};
  for (const {cmd, by} of cmds) {
    switch (cmd) {
      case 'N': {
        waypoint.y += by;
        break;
      }
      case 'S': {
        waypoint.y -= by;
        break;
      }
      case 'E': {
        waypoint.x += by;
        break;
      }
      case 'W': {
        waypoint.x -= by;
        break;
      }
      case 'L': {
        for (let i = 0; i < by / 90; ++i) {
          const ny = waypoint.x;
          const nx = -waypoint.y;
          waypoint.x = nx;
          waypoint.y = ny;
        }
        break;
      }
      case 'R': {
        for (let i = 0; i < by / 90; ++i) {
          const ny = -waypoint.x;
          const nx = waypoint.y;
          waypoint.x = nx;
          waypoint.y = ny;
        }
        break;
      }
      case 'F': {
        pos.x += waypoint.x * by;
        pos.y += waypoint.y * by;
        break;
      }
    }
  }
  return pos;
}

console.error(`Part 2:`);
{
  const {x, y} = route2(dirs);
  console.log(Math.abs(x) + Math.abs(y));
}
