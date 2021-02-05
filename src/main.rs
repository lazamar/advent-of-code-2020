#![feature(btree_drain_filter)]
#![feature(iterator_fold_self)]
#[macro_use]
extern crate scan_fmt;
mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let day = args.iter().nth(1).map(|v| v.as_str());
    match day {
        Some("1") => day1::main(),
        Some("2") => day2::main(),
        Some("3") => day3::main(),
        Some("4") => day4::main(),
        Some("5") => day5::main(),
        Some("6") => day6::main(),
        _ => println!("Choose a day."),
    }
}
