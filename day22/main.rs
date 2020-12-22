use std::collections::HashSet;
use std::collections::VecDeque;
use std::fs;
use std::hash::{Hash, Hasher};

type Card = usize;

#[derive(Debug, Hash, PartialEq, Eq)]
struct Player {
    deck: VecDeque<Card>,
}

fn parse_2_players(s: &str) -> (Player, Player) {
    let mut players = s.split("\n\n").map(Player::parse);
    let p1 = players.next().unwrap();
    let p2 = players.next().unwrap();
    (p1, p2)
}

impl Player {
    fn parse(s: &str) -> Self {
        Self {
            deck: s
                .lines()
                .skip(/* player name */ 1)
                .map(|l| l.parse().unwrap())
                .collect(),
        }
    }

    fn score(&self) -> usize {
        self.deck
            .iter()
            .rev()
            .zip(1..)
            .fold(0, |total, (p, m)| total + p * m)
    }

    fn is_out(&self) -> bool {
        self.deck.is_empty()
    }

    fn draw(&mut self) -> Card {
        self.deck.pop_front().unwrap()
    }

    fn add(&mut self, card: Card) -> &mut Self {
        self.deck.push_back(card);
        self
    }

    fn num_cards(&self) -> usize {
        self.deck.len()
    }

    fn copy_n(&self, n: usize) -> Self {
        Self {
            deck: self.deck.iter().take(n).map(|n| *n).collect(),
        }
    }
}

fn combat(mut p1: Player, mut p2: Player) -> Player {
    while !p1.is_out() && !p2.is_out() {
        let (top1, top2) = (p1.draw(), p2.draw());
        if top1 > top2 {
            p1.add(top1).add(top2);
        } else {
            p2.add(top2).add(top1);
        }
    }
    if p1.is_out() {
        p2
    } else {
        p1
    }
}

fn h(p1: &Player, p2: &Player) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    p1.hash(&mut hasher);
    p2.hash(&mut hasher);
    hasher.finish()
}

enum Winner {
    Player1,
    Player2,
}

fn combat_recursive(mut p1: Player, mut p2: Player) -> (Winner, Player) {
    let mut seen = HashSet::new();
    while !p1.is_out() && !p2.is_out() {
        let roundh = h(&p1, &p2);
        if seen.contains(&roundh) {
            return (Winner::Player1, p1);
        }
        seen.insert(roundh);

        let (top1, top2) = (p1.draw(), p2.draw());
        if top1 <= p1.num_cards() && top2 <= p2.num_cards() {
            match combat_recursive(p1.copy_n(top1), p2.copy_n(top2)) {
                (Winner::Player1, _) => p1.add(top1).add(top2),
                (Winner::Player2, _) => p2.add(top2).add(top1),
            };
        } else if top1 > top2 {
            p1.add(top1).add(top2);
        } else {
            p2.add(top2).add(top1);
        }
    }
    if p1.is_out() {
        (Winner::Player2, p2)
    } else {
        (Winner::Player1, p1)
    }
}

fn main() {
    let fi = fs::read_to_string("day22/input.txt").unwrap();

    {
        let (p1, p2) = parse_2_players(&fi);
        println!("{}", combat(p1, p2).score());
    }

    {
        let (p1, p2) = parse_2_players(&fi);
        println!("{}", combat_recursive(p1, p2).1.score());
    }
}
