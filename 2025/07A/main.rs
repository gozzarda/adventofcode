use std::io;

fn main() {
    let stdin = io::stdin();

    let mut num_splits: u64 = 0;
    let mut beams: Vec<bool> = vec![];
    for line in stdin.lines() {
        let line: Vec<char> = line.unwrap().chars().collect();
        beams.resize(line.len(), false);
        let mut r = beams[1];
        for i in 1..(beams.len() - 1) {
            let m = r;
            r = beams[i + 1];
            match line[i] {
                '.' => {}
                '^' => {
                    if m {
                        num_splits += 1;
                        beams[i - 1] = true;
                        beams[i] = false;
                        beams[i + 1] = true;
                    }
                }
                'S' => {
                    beams[i] = true;
                }
                _ => {}
            }
        }
    }

    println!("{}", num_splits);
}
