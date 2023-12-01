use std::io;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let sum = lines
        .iter()
        .map(|line| {
            let digits: Vec<u32> = line.chars().filter_map(|c| c.to_digit(10)).collect();
            let head = digits.first()?;
            let last = digits.last()?;
            Some(head * 10 + last)
        })
        .sum::<Option<u32>>()
        .unwrap();
    println!("{}", sum);
}
