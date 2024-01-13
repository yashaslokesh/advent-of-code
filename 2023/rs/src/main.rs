mod day_01;
use clap::Parser;

#[derive(Parser, Debug)]
struct CliArgs {
    day: u8,
}

fn main() {
    let args = CliArgs::parse();

    match args.day {
        1 => day_01::run(),
        _ => panic!("Not a valid day")
    }
}
