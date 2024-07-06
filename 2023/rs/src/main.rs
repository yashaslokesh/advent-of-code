mod day_01;
use clap::Parser;
use std::fs::read_to_string;

#[derive(Parser, Debug)]
struct CliArgs {
    day: u8,
}

fn main() {
    let args = CliArgs::parse();
    
    let lines: Vec<String> = read_to_string("../inputs/01.txt")
        .unwrap()
        .lines()
        .map(String::from)
        .collect();

    match args.day {
        1 => day_01::run(&lines),
        _ => panic!("Not a valid day")
    }
}
