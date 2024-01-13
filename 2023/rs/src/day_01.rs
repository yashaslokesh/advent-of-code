use std::fs::read_to_string;

fn a() {
    let lines: Vec<String> = read_to_string("../inputs/01a.txt")
        .unwrap()
        .lines()
        .map(|s| String::from)
        .collect();

    let sum = 0;
    for line in &lines {
        let chars = line.chars();
        for char in chars {
            if char.is_digit(10) {
                
            }
        }
    }

    println!("{:?}", &lines);
}

fn b() {
    
}

pub fn run() {
    a();
    b();
}