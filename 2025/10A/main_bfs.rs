use std::io::{self, BufRead};
use std::collections::VecDeque;

fn main() {
    let stdin = io::stdin().lock();

    let mut total: u64 = 0;
    for line in stdin.lines() {
        let line = line.unwrap();
        let (lights, line) = line.split_once(' ').unwrap();
        let (buttons, _joltages) = line.rsplit_once(' ').unwrap();

        // Convert lights to bitset
        let lights = lights.trim_matches(|c| c == '[' || c == ']');
        let num_lights = lights.len();
        let lights: usize = lights
            .chars()
            .map(|c| if c == '#' { 1 } else { 0 })
            .enumerate()
            .fold(0, |acc, (ind, bit)| acc | (bit << ind));

        // Convert buttons to bitsets
        let buttons: Vec<usize> = buttons
            .split_whitespace()
            .map(|button| {
                button
                    .trim_matches(|c| c == '(' || c == ')')
                    .split(',')
                    .map(|ind| ind.parse().unwrap())
                    .fold(0, |acc, ind: u8| acc | (1 << ind))
            })
            .collect();

        let num_states: usize = 1 << num_lights;
        let mut dist: Vec<u8> = vec![u8::MAX; num_states];
        let mut queue: VecDeque<usize> = VecDeque::new();

        dist[0] = 0;
        queue.push_back(0);
        while let Some(curr) = queue.pop_front() {
            for button in &buttons {
                let next = curr ^ button;
                if dist[next] == u8::MAX {
                    dist[next] = dist[curr] + 1;
                    queue.push_back(next);
                }
            }
        }

        total += dist[lights] as u64;
    }

    println!("{}", total);
}
