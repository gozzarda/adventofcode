use std::io::{self, BufRead};

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
        let sentinel: u8 = buttons.len() as u8 + 1;
        let mut dpt: Vec<u8> = vec![sentinel; num_states];
        dpt[0] = 0;
        for button in buttons {
            for state in 0..num_states {
                dpt[state] = dpt[state].min(1 + dpt[state ^ button]);
            }
        }

        assert!(dpt[lights] != sentinel);
        total += dpt[lights] as u64;
    }

    println!("{}", total);
}
