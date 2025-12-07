use std::io;

fn main() {
    let stdin = io::stdin();

    let mut beams: Vec<u64> = vec![];
    for line in stdin.lines() {
        let line: Vec<char> = line.unwrap().chars().collect();
        beams.resize(line.len(), 0);
        let mut r = beams[1];
        for i in 1..(beams.len() - 1) {
            let m = r;
            r = beams[i + 1];
            match line[i] {
                '.' => {}
                '^' => {
                    beams[i - 1] += m;
                    beams[i] -= m;
                    beams[i + 1] += m;
                }
                'S' => {
                    beams[i] += 1;
                }
                _ => {}
            }
        }
    }

    let num_beams: u64 = beams.iter().sum();

    println!("{}", num_beams);
}
