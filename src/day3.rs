use std::fs;
use std::str::Lines;

const tree: char = '#';

pub fn main() {
    let file = fs::read_to_string("puzzle-inputs/3.txt").expect("Unable to read input file");
    let initial_strategy = Strategy { down: 1, right: 3 };

    println!(
        "Trees hit using strategy one: {}",
        count_trees(file.lines(), &initial_strategy)
    );

    let strategies = vec![
        Strategy { down: 1, right: 1 },
        Strategy { down: 1, right: 3 },
        Strategy { down: 1, right: 5 },
        Strategy { down: 1, right: 7 },
        Strategy { down: 2, right: 1 },
    ];

    let multiplied_trees = strategies
        .iter()
        .map(|s| count_trees(file.lines(), &s))
        .fold(1, |acc, v| acc * v);

    println!(
        "Trees hit using multiple strategies multiplied: {}",
        multiplied_trees
    );
}

struct Strategy {
    down: u8,
    right: u8,
}

fn count_trees(lines: Lines, strategy: &Strategy) -> usize {
    lines
        .enumerate()
        .map(|(ix, line)| hits_tree(ix, line, strategy))
        .map(|hit| if hit { 1 } else { 0 })
        .sum()
}

fn hits_tree(ix: usize, line: &str, s: &Strategy) -> bool {
    if (ix % (s.down as usize)) != 0 {
        return false;
    }

    let down_rounds = ix / s.down as usize;
    let right_position = down_rounds * s.right as usize;
    let pass_ix = right_position % line.len();
    line.chars().nth(pass_ix) == Some(tree)
}
