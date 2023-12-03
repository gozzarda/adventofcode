use std::cmp::{max, min};
use std::io;

fn main() {
    let stdin = io::stdin();
    let grid = stdin
        .lines()
        .map(|l| l.unwrap().chars().collect::<Vec<char>>())
        .collect::<Vec<_>>();
    let mut total: u32 = 0;
    for row in 0..grid.len() {
        let row_lwr = max(1, row) - 1;
        let row_upr = min(row + 2, grid.len());
        for col in 0..grid[row].len() {
            if col > 0 && grid[row][col - 1].is_digit(10) {
                continue;
            }
            let len = grid[row][col..]
                .iter()
                .take_while(|c| c.is_digit(10))
                .count();
            if len == 0 {
                continue;
            }
            let val = grid[row][col..col + len]
                .iter()
                .collect::<String>()
                .parse::<u32>()
                .unwrap();
            let col_lwr = max(1, col) - 1;
            let col_upr = min(col + len + 1, grid[row].len());
            'search: for r in row_lwr..row_upr {
                for c in col_lwr..col_upr {
                    if grid[r][c] != '.' && grid[r][c].is_ascii_punctuation() {
                        total += val;
                        break 'search;
                    }
                }
            }
        }
    }
    println!("{}", total);
}
