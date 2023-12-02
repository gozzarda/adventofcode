use std::io;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let sum: u32 = lines
        .iter()
        .filter_map(|line| {
            let (head, tail) = line
                .split_once(':')
                .expect("line should start with a \"Game <id>:\" header");
            let hands = tail.split(';').map(|s| s.trim());
            for hand in hands {
                let cubes = hand.split(',').map(|s| s.trim());
                for cube in cubes {
                    let (num_str, colour) = cube
                        .trim()
                        .split_once(' ')
                        .expect("number and colour should be space separated");
                    let num = num_str.parse::<u32>().expect("first word should be an int");
                    let limit = match colour {
                        "red" => 12,
                        "green" => 13,
                        "blue" => 14,
                        _ => panic!("colour should be one of red, green, or blue"),
                    };
                    if num > limit {
                        return None;
                    }
                }
            }
            let id = head
                .chars()
                .filter(|c| c.is_digit(10))
                .collect::<String>()
                .parse::<u32>()
                .expect("header should contain interger id");
            Some(id)
        })
        .sum();
    println!("{}", sum);
}
