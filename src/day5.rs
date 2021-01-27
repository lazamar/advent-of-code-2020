use std::collections::BTreeSet;
use std::fs;

pub fn main() {
    let file = fs::read_to_string("puzzle-inputs/5.txt").expect("Unable to read input file");
    let seat_ids: Vec<u32> = file.lines().map(seat_id).collect();
    let max_seat_id = seat_ids.iter().max().unwrap();
    println!("Max seat id: {}", max_seat_id);

    let min_id = *seat_ids.iter().min().unwrap();
    let max_id = *seat_ids.iter().max().unwrap();
    for i in min_id..max_id {
        let this = seat_ids.contains(&i);
        let prev = seat_ids.contains(&(i - 1));
        let next = seat_ids.contains(&(i + 1));
        if !this && prev && next {
            println!("My seat ID: {}", i);
        }
    }
}

fn seat_id(input: &str) -> u32 {
    let from_char = |c| match c {
        'L' => 0,
        'R' => 1,
        'F' => 0,
        'B' => 1,
        _ => panic!("Unexpected char {}", c),
    };
    let row = input[0..7].chars().fold(0, |acc, c| acc * 2 + from_char(c));
    let col = input[7..].chars().fold(0, |acc, c| acc * 2 + from_char(c));
    row * 8 + col
}
