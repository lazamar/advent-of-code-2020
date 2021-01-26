use std::collections::HashMap;
use std::fs;
use std::iter::Iterator;

pub fn main() {
    let file = fs::read_to_string("puzzle-inputs/4.txt").expect("Unable to read input file");
    let passports = to_passports(&file);
    println!(
        "Passports valid for criteria 1: {}",
        passports.iter().filter(|p| validate_1(p)).count()
    );
    println!(
        "Passports valid for criteria 2: {}",
        passports
            .iter()
            .filter(|p| REQ_FIELDS.iter().all(|&f| p.contains_key(f)))
            .filter(|p| validate_2(p))
            .count()
    );
}

type Passport = HashMap<String, String>;

fn to_passports(content: &String) -> Vec<Passport> {
    content
        .split("\n\n")
        .map(|line| {
            line.split_ascii_whitespace()
                .map(|w| scan_fmt!(w, "{}:{}", String, String).unwrap())
                .collect()
        })
        .collect()
}

// "cid" is not required
const REQ_FIELDS: [&'static str; 7] = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];

fn validate_1(p: &Passport) -> bool {
    for &key in REQ_FIELDS.iter() {
        if !p.contains_key(key) {
            return false;
        }
    }
    return true;
}

const EYE_COLORS: [&'static str; 7] = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];

fn validate_2(p: &Passport) -> bool {
    let checks = [
        p.get("byr")
            .and_then(|v| scan_fmt!(v, "{d}", u32).ok())
            .map(|y| 1920 <= y && y <= 2002),
        p.get("iyr")
            .and_then(|v| scan_fmt!(v, "{d}", u32).ok())
            .map(|y| 2010 <= y && y <= 2020),
        p.get("eyr")
            .and_then(|v| scan_fmt!(v, "{d}", u32).ok())
            .map(|y| 2020 <= y && y <= 2030),
        p.get("hgt").map(|v| {
            if let Ok((n, _)) = scan_fmt!(&v, "{3d}c{[m]}", u32, char) {
                return 150 <= n && n <= 193 && v.len() == 5;
            }
            if let Ok((n, _)) = scan_fmt!(&v, "{2d}i{[n]}", u32, char) {
                return 59 <= n && n <= 76 && v.len() == 4;
            }
            false
        }),
        p.get("hcl")
            .map(|v| scan_fmt!(v, "#{6[0-9a-f]}", String).is_ok()),
        p.get("ecl").map(|v| EYE_COLORS.iter().any(|c| c == v)),
        p.get("pid")
            .map(|v| v.chars().all(|c| c.is_digit(10)) && v.len() == 9),
    ];
    checks.iter().all(|v| v.unwrap_or(false))
}
