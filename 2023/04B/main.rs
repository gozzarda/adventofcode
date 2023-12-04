use std::collections::HashSet;
use std::io;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let scores: Vec<usize> = lines
        .iter()
        .map(|line| {
            let (_, body) = line.split_once(':').unwrap();
            let (lhs, rhs) = body.split_once('|').unwrap();
            let lvals: HashSet<u32> = lhs.split(' ').filter_map(|w| w.parse().ok()).collect();
            let rvals: HashSet<u32> = rhs.split(' ').filter_map(|w| w.parse().ok()).collect();
            lvals.intersection(&rvals).count()
        })
        .collect();
    let mut counts: Vec<usize> = vec![1; scores.len()];
    for (i, score) in scores.iter().enumerate() {
        for j in 1..=*score {
            counts[i + j] += counts[i]
        }
    }
    let total: usize = counts.iter().sum();
    println!("{}", total);
}
