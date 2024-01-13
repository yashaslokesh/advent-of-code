use std::fs::read_to_string;

fn a() {
    let lines: Vec<String> = read_to_string("../inputs/01a.txt")
        .unwrap()
        .lines()
        .map(String::from)
        .collect();

    let mut sum = 0;
    for line in &lines {
        sum += line.chars().find(|c| c.is_digit(10)).unwrap().to_digit(10).unwrap() * 10 + 
            line.chars().rev().find(|c| c.is_digit(10)).unwrap().to_digit(10).unwrap();
    }

    println!("{:?}", sum);
}

fn b() {
    
}

pub fn run() {
    a();
    b();
}