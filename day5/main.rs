use std::collections::HashSet;
use std::fs;

fn get_row(line: &str) -> usize {
    let mut lo = 0;
    let mut hi = 127;
    for ch in line.chars() {
        match ch {
            'F' => hi = (hi + lo) / 2,
            'B' => lo = (hi + lo) / 2 + 1,
            _ => unreachable!(),
        }
    }
    match line.chars().last().unwrap() {
        'F' => lo,
        'B' => hi,
        _ => unreachable!(),
    }
}

fn get_col(line: &str) -> usize {
    let mut lo = 0;
    let mut hi = 7;
    for ch in line.chars() {
        match ch {
            'L' => hi = (hi + lo) / 2,
            'R' => lo = (hi + lo) / 2 + 1,
            _ => unreachable!(),
        }
    }
    match line.chars().last().unwrap() {
        'L' => lo,
        'R' => hi,
        _ => unreachable!(),
    }
}

#[derive(Debug)]
struct Seat {
    row: usize,
    col: usize,
}
impl Seat {
    fn new(line: &&str) -> Self {
        Self {
            row: get_row(&line[0..7]),
            col: get_col(&line[7..]),
        }
    }
    fn id(&self) -> usize {
        self.row * 8 + self.col
    }
}

fn main() {
    let fi = fs::read_to_string("day5/input.txt").unwrap();
    let lines = fi.lines().collect::<Vec<_>>();

    let seats = lines.iter().map(Seat::new).collect::<Vec<_>>();
    let ids = seats.iter().map(Seat::id).collect::<HashSet<_>>();

    let max_id = ids.iter().max().unwrap();
    println!("part 1: {}", max_id);

    for row in 0..128 {
        for col in 0..8 {
            let id = Seat { row, col }.id();
            if !ids.contains(&id) && ids.contains(&(id + 1)) && ids.contains(&(id - 1)) {
                println!("part 2: {}", id);
            }
        }
    }
}
