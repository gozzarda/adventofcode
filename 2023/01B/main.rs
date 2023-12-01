use std::io;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let sum = lines
        .iter()
        .map(|line| {
            // I *should* use find and rfind to search for pattern matches specified by index and
            // value and select the leftmost and rightmost ones...
            // Or I could just hackily modify the string so that it contains the digits now.
            // Bonus ugly because we don't want to break any overlapping patterns.
            let line = line
                .replace("one", "o1e")
                .replace("two", "t2o")
                .replace("three", "t3e")
                .replace("four", "4")
                .replace("five", "5e")
                .replace("six", "6")
                .replace("seven", "7n")
                .replace("eight", "e8t")
                .replace("nine", "n9e");
            let digits: Vec<u32> = line.chars().filter_map(|c| c.to_digit(10)).collect();
            let head = digits.first()?;
            let last = digits.last()?;
            Some(head * 10 + last)
        })
        .sum::<Option<u32>>()
        .unwrap();
    println!("{}", sum);
}
