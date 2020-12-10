import * as fs from 'fs';

const lines = fs.readFileSync('day4/input.txt').toString().split('\n');
lines.pop();  // trailing newline

interface Passport {
  byr: string;
  iyr: string;
  eyr: string;
  hgt: string;
  hcl: string;
  ecl: string;
  pid: string;
  cid?: string;
}

const passportData: Array<Partial<Passport>> = [];
{
  let datum = {};
  for (const line of lines) {
    if (line === '') {
      passportData.push(datum);
      datum = {};
      continue;
    }
    for (const field of line.split(' ')) {
      const [k, v] = field.split(':');
      datum[k] = v;
    }
  }
  passportData.push(datum);
}

type CorrectKeyedPassport = {
  [P in keyof Passport]: string
};
function validate_keys(passport: Partial<Passport>):
    passport is CorrectKeyedPassport {
  for (const key of ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid'])
    if (!(key in passport)) return false;
  return true;
}

{
  console.error(`part 1:`);
  console.log(passportData.filter(validate_keys).length);
}

const within = (n: number, lo: number, hi: number) => n >= lo && n <= hi;
function validate_passport(
    {byr, iyr, eyr, hgt, hcl, ecl, pid}: Partial<Passport>): boolean {
  // byr
  return byr && byr.length === 4 && within(Number(byr), 1920, 2002) &&
      // iyr
      iyr && iyr.length === 4 && within(Number(iyr), 2010, 2020) &&
      // eyr
      eyr && eyr.length === 4 && within(Number(eyr), 2020, 2030) &&
      // hgt
      hgt &&
      (hgt.endsWith('cm') && hgt.length === 5 &&
           within(Number(hgt.substring(0, 3)), 150, 193) ||
       hgt.endsWith('in') && hgt.length === 4 &&
           within(Number(hgt.substring(0, 2)), 59, 76)) &&
      // hcl
      hcl && /^#[0-9a-fA-F]{6}$/.exec(hcl) !== null &&
      // ecl
      ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'].includes(ecl) &&
      // pid
      pid && /^\d{9}$/.exec(pid) !== null;
}

{
  console.error(`part 2:`);
  console.log(passportData.filter(validate_passport).length);
}
