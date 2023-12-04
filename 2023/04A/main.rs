use std::collections::HashSet;
use std::io;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let score: u32 = lines
        .iter()
        .map(|line| {
            let (_, body) = line.split_once(':').unwrap();
            let (lhs, rhs) = body.split_once('|').unwrap();
            let lvals: HashSet<u32> = lhs.split(' ').filter_map(|w| w.parse().ok()).collect();
            let rvals: HashSet<u32> = rhs.split(' ').filter_map(|w| w.parse().ok()).collect();
            let matches: usize = lvals.intersection(&rvals).count();
            if matches > 0 {
                u32::pow(2, (matches as u32) - 1)
            } else {
                0
            }
        })
        .sum();
    println!("{}", score);
}
