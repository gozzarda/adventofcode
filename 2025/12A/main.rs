// Hackjob relying on the input being trivial
// May come back later to do a proper heuristic-guided search

#![feature(slice_split_once)]

use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin().lock();

    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();

    let (shapes, regions) = lines.rsplit_once(|l| l.is_empty()).unwrap();

    let shapes: Vec<Vec<Vec<bool>>> = shapes
        .split(|l| l.is_empty())
        .map(|ls| {
            ls[1..]
                .iter()
                .map(|l| l.chars().map(|c| c == '#').collect())
                .collect()
        })
        .collect();

    let regions: Vec<((usize, usize), Vec<usize>)> = regions
        .iter()
        .map(|l| {
            let (dims, counts) = l.split_once(':').unwrap();
            let dims = dims.split_once('x').unwrap();
            let dims = (dims.0.parse().unwrap(), dims.1.parse().unwrap());
            let counts = counts
                .trim()
                .split(' ')
                .map(|w| w.parse().unwrap())
                .collect();
            (dims, counts)
        })
        .collect();

    let sizes: Vec<usize> = shapes
        .into_iter()
        .map(|s| s.into_iter().flatten().filter(|&b| b).count())
        .collect();

    let result = regions
        .into_iter()
        .filter(|((w, h), ns)| {
            let size: usize = ns.iter().zip(sizes.iter()).map(|(n, s)| n * s).sum();
            let area: usize = w * h;
            size <= area
        })
        .count();

    println!("{}", result);
}
