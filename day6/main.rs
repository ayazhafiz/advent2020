use std::collections::HashSet;
use std::fs;

fn main() {
    let fi = fs::read_to_string("day6/input.txt").unwrap();
    let lines = fi.lines().collect::<Vec<_>>();

    let mut sum = 0;
    let mut questions = HashSet::new();
    for line in lines.iter() {
        if line.is_empty() {
            sum += questions.len();
            questions.clear();
            continue;
        }
        for q in line.chars() {
            questions.insert(q);
        }
    }
    sum += questions.len();
    questions.clear();
    println!("part 1: {}", sum);

    let mut sum = 0;
    let mut first = true;
    let mut questions = HashSet::new();
    for line in lines.iter() {
        if line.is_empty() {
            sum += questions.len();
            first = true;
            questions.clear();
            continue;
        }
        let mut fresh = HashSet::new();
        for q in line.chars() {
            if first || questions.contains(&q) {
                fresh.insert(q);
            }
        }
        first = false;
        questions = fresh;
    }
    sum += questions.len();
    questions.clear();
    println!("part 2: {}", sum);
}
