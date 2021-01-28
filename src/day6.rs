use std::collections::BTreeSet;
use std::fs;

pub fn main() {
    print!("Yeah");

    let file = fs::read_to_string("puzzle-inputs/6.txt").expect("Unable to read input file");

    let line_to_answers = |line: &str| -> BTreeSet<char> { line.chars().collect() };

    type Group = BTreeSet<char>;

    let groups: Vec<Vec<Group>> = file
        .split("\n\n")
        .map(|a| a.split_ascii_whitespace().map(line_to_answers).collect())
        .collect();

    let any_answer = |group: &Vec<Group>| {
        group
            .iter()
            .fold(BTreeSet::new(), |mut a: Group, b| {
                a.extend(b);
                a
            })
            .len()
    };

    println!(
        "Sum of different questions asked by each group: {}",
        groups.iter().map(any_answer).sum::<usize>()
    );

    let all_answers = |group: &Vec<Group>| {
        group
            .iter()
            .fold(group[0].clone(), |a: Group, b: &Group| {
                a.intersection(b).cloned().collect()
            })
            .len()
    };

    println!(
        "Sum of equal questions asked by each group: {}",
        groups.iter().map(all_answers).sum::<usize>()
    );
}
