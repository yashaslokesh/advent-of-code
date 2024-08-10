mod day_01;
mod day_02;
use clap::Parser;
use std::fs::read_to_string;

#[derive(Parser, Debug)]
struct CliArgs {
    day: u8,
}

fn get_lines(day: u8) -> Vec<String> {
    format!("../inputs/{}.txt", format!("{:0>2}", day));
    let lines: Vec<String> = read_to_string("../inputs/01.txt")
        .unwrap()
        .lines()
        .map(String::from)
        .collect();

    lines
}

fn main() {
    let args = CliArgs::parse();
    
    let lines = get_lines(args.day);

    match args.day {
        1 => day_01::run(&lines),
        2 => day_02::run(&lines),
        _ => panic!("Not a valid day")
    }
}
