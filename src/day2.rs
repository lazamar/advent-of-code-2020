use pom::char_class::{alpha, digit, multispace, space};
use pom::parser::Parser;
use pom::parser::{is_a, list};
use std::char;
use std::fmt::Debug;
use std::fs;
use std::string::String;

pub fn main() {
    let file = fs::read("puzzle-inputs/2.txt").expect("Unable to read input file");
    let policies_and_passwords: Vec<(Policy, String)> = to_policies(&file);

    {
        let mut valid = 0;
        for (pol, pass) in policies_and_passwords.iter() {
            if is_valid_1(pol, pass) {
                valid += 1;
            }
        }
        println!("Problem 1 valid passwords: {}", valid);
    }

    {
        let mut valid = 0;
        for (pol, pass) in policies_and_passwords.iter() {
            if is_valid_2(pol, pass) {
                valid += 1;
            }
        }
        println!("Problem 2 valid passwords: {}", valid);
    }
}

fn is_valid_1(policy: &Policy, pass: &String) -> bool {
    let count = pass.matches(policy.character).collect::<Vec<_>>().len();
    (policy.low as usize) <= count && count <= (policy.high as usize)
}

fn is_valid_2(policy: &Policy, pass: &String) -> bool {
    let matches = |pos: u8| {
        pass.chars()
            .nth(pos as usize - 1)
            .map(|v| v == policy.character)
            .unwrap()
    };
    let xor = |a: bool, b: bool| (a && !b) || (b && !a);

    xor(matches(policy.low), matches(policy.high))
}

fn to_policies(s: &Vec<u8>) -> Vec<(Policy, String)> {
    let is = move |c| move |d| d == c as u8;

    let p_password = string_of(alpha);
    let p_line = policy_parser() - character(is(':')) - character(space) + p_password;
    let p = list(p_line, is_a(multispace));
    p.parse(s).unwrap()
}

#[derive(Debug)]
struct Policy {
    low: u8,
    high: u8,
    character: char,
}

fn policy_parser<'a>() -> Parser<'a, u8, Policy> {
    let is = move |c| move |d| d == c as u8;
    let args: Parser<'a, u8, ((u8, u8), char)> =  // --
        number()
        - character(is('-'))
        + number()
        - character(space)
        + character(alpha);

    args.map(|((low, high), character)| Policy {
        low,
        high,
        character,
    })
}

fn number<'a>() -> Parser<'a, u8, u8> {
    is_a(digit)
        .repeat(1..)
        .convert(String::from_utf8)
        .convert(|s| u8::from_str_radix(&s, 10))
        .name("number")
}

fn character<'a, P>(predicate: P) -> Parser<'a, u8, char>
where
    P: Fn(u8) -> bool + 'a,
{
    is_a(predicate).map(|c| c as char).name("character")
}

fn string_of<'a, P>(predicate: P) -> Parser<'a, u8, String>
where
    P: Fn(u8) -> bool + 'a,
{
    is_a(predicate)
        .repeat(1..)
        .convert(String::from_utf8)
        .name("string")
}
