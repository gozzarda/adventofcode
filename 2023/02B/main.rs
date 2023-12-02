use std::cmp::max;
use std::io;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let sum: u32 = lines
        .iter()
        .map(|line| {
            let (_, tail) = line
                .split_once(':')
                .expect("line should start with a \"Game <id>:\" header");
            let hands = tail.split(';').map(|s| s.trim());
            let mut num_r: u32 = 0;
            let mut num_g: u32 = 0;
            let mut num_b: u32 = 0;
            for hand in hands {
                let cubes = hand.split(',').map(|s| s.trim());
                for cube in cubes {
                    let (num_str, colour) = cube
                        .trim()
                        .split_once(' ')
                        .expect("number and colour should be space separated");
                    let num = num_str.parse::<u32>().expect("first word should be an int");
                    match colour {
                        "red" => num_r = max(num_r, num),
                        "green" => num_g = max(num_g, num),
                        "blue" => num_b = max(num_b, num),
                        _ => panic!("colour should be one of red, green, or blue"),
                    };
                }
            }
            let power = num_r * num_g * num_b;
            power
        })
        .sum();
    println!("{}", sum);
}
