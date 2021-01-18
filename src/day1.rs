use std::collections::BTreeSet;
use std::fs;
use std::iter::Iterator;
use std::str::FromStr;

pub fn main() {
    let file = fs::read_to_string("puzzle-inputs/1.txt").expect("Unable to read input file");

    let to_i32 = |n| i32::from_str(n).expect(&format!("Unable to convert {} to i32", n));

    let numbers: BTreeSet<i32> = file.lines().map(to_i32).collect();

    // Part 1
    let (one, two) = two_numbers_that_sum_to(2020, &numbers).expect("Unable to find two numbers");
    println!("Two numbers: {}, {}. Multiply to {}", one, two, one * two);

    // Part 2
    let (one, two, three) =
        three_numbers_that_sum_to(2020, &numbers).expect("Unable to find three numbers");
    println!(
        "Three numbers numbers: {}, {}, {}. Multiply to {}",
        one,
        two,
        three,
        one * two * three
    );
}

fn two_numbers_that_sum_to(res: i32, numbers: &BTreeSet<i32>) -> Option<(&i32, &i32)> {
    let mut first = None;
    for n in numbers {
        if numbers.contains(&(res - n)) {
            match first {
                None => first = Some(n),
                Some(nn) => {
                    if n != nn {
                        return Some((n, nn));
                    }
                }
            }
        }
    }
    None
}

fn three_numbers_that_sum_to(res: i32, numbers: &BTreeSet<i32>) -> Option<(&i32, &i32, &i32)> {
    for n in numbers {
        match two_numbers_that_sum_to(res - n, numbers) {
            None => (),
            Some((one, two)) => return Some((one, two, n)),
        }
    }
    None
}
