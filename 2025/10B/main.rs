use std::collections::HashMap;
use std::io::{self, BufRead};

fn solve(
    dpt: &mut HashMap<Vec<i64>, Option<u64>>,
    actions: &Vec<Vec<(u64, Vec<i64>)>>,
    joltages: Vec<i64>,
) -> Option<u64> {
    if let Some(&result) = dpt.get(&joltages) {
        return result;
    }
    if joltages.iter().any(|j| *j < 0) {
        return None;
    }
    if joltages.iter().all(|j| *j == 0) {
        return Some(0);
    }
    let parity: usize = joltages
        .iter()
        .rev()
        .map(|j| *j & 1)
        .fold(0, |acc, bit| (acc << 1) | (bit as usize));
    let result = actions[parity]
        .iter()
        .filter_map(|(cost, delta)| {
            let remain: Vec<i64> = joltages
                .iter()
                .zip(delta.iter())
                .map(|(&lhs, &rhs)| lhs - rhs)
                .map(|j| j / 2)
                .collect();
            solve(dpt, actions, remain).map(|n| 2 * n + cost)
        })
        .min();
    dpt.insert(joltages.clone(), result);
    result
}

fn main() {
    let stdin = io::stdin().lock();

    let mut total: u64 = 0;
    for line in stdin.lines() {
        let line = line.unwrap();
        let (_lights, line) = line.split_once(' ').unwrap();
        let (buttons, joltages) = line.rsplit_once(' ').unwrap();

        let buttons: Vec<Vec<usize>> = buttons
            .split_whitespace()
            .map(|button| {
                button
                    .trim_matches(|c| c == '(' || c == ')')
                    .split(',')
                    .map(|ind| ind.parse().unwrap())
                    .collect()
            })
            .collect();

        let joltages: Vec<i64> = joltages
            .trim_matches(|c| c == '{' || c == '}')
            .split(',')
            .map(|val| val.parse().unwrap())
            .collect();

        let num_parities = 1 << joltages.len();
        let mut actions: Vec<Vec<(u64, Vec<i64>)>> = vec![vec![]; num_parities];
        let num_subsets = 1 << buttons.len();
        for subset in 0..num_subsets {
            let mut cost: u64 = 0;
            let mut delta: Vec<i64> = vec![0; joltages.len()];
            for (i, button) in buttons.iter().enumerate() {
                if subset & (1 << i) != 0 {
                    cost += 1;
                    for &j in button {
                        delta[j] += 1;
                    }
                }
            }
            let parity: usize = delta
                .iter()
                .rev()
                .map(|d| *d & 1)
                .fold(0, |acc, bit| (acc << 1) | (bit as usize));
            actions[parity].push((cost, delta));
        }

        let mut dpt: HashMap<Vec<i64>, Option<u64>> = HashMap::new();
        let result = solve(&mut dpt, &actions, joltages);

        total += result.unwrap();
    }

    println!("{}", total);
}
