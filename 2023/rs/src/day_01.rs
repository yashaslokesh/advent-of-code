fn a(lines: &Vec<String>) -> u32 {
    let mut sum = 0;
    for line in lines {
        sum += line.chars().find(|c| c.is_digit(10)).unwrap().to_digit(10).unwrap() * 10 + 
            line.chars().rev().find(|c| c.is_digit(10)).unwrap().to_digit(10).unwrap();
    }

    sum
}

fn match_with_num(s: &str) -> Result<u16, u16> {
    let res = match s {
        "one" => Ok(1),
        "two" => Ok(2),
        "three" => Ok(3),
        "four" => Ok(4),
        "five" => Ok(5),
        "six" => Ok(6),
        "seven" => Ok(7),
        "eight" => Ok(8),
        "nine" => Ok(9),
        _ => Err(0)
    };

    res
}

fn process(s: &String, reverse: bool) -> u16 {
    for i in 0..s.len() {
        for j in i..s.len() {
            let substr: String;
            if !reverse {
                substr = s[i..=j].to_string();
            } else {
                substr = s[i..=j].chars().rev().collect::<String>();
            };

            if substr.len() == 1 {
                let res = substr.parse::<u16>().or_else(|_| match_with_num(&substr));
                if let Ok(d) = res { return d; }
            } else {
                let res = match_with_num(&substr);
                if let Ok(d) = res { return d; }
            }
        }
    }
    0
}

fn b(lines: &Vec<String>) -> u16 {
    let mut sum: u16 = 0;
    for line in lines {
        sum += process(&line, false) * 10 + process(&line.chars().rev().collect::<String>(), true);
    }
    sum
}

pub fn run(input: &Vec<String>) {
    println!("Day 1a: {}", a(&input));
    println!("Day 1b: {}", b(&input));
}