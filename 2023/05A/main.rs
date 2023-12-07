use std::io;

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let paras = lines.split(|l| l.is_empty()).collect::<Vec<_>>();
    let (seeds_para, map_paras) = paras.split_first().unwrap();
    let seeds: Vec<usize> = seeds_para
        .first()
        .unwrap()
        .strip_prefix("seeds: ")
        .unwrap()
        .split(" ")
        .map(|w| w.parse().unwrap())
        .collect();
    let maps: Vec<Vec<(usize, usize, usize)>> = map_paras
        .iter()
        .map(|p| {
            p.iter()
                .skip(1)
                .map(|l| {
                    let mut it = l.split(" ").map(|w| w.parse().unwrap());
                    (it.next().unwrap(), it.next().unwrap(), it.next().unwrap())
                })
                .collect()
        })
        .collect();
    let locs = seeds.iter().map(|seed| {
        maps.iter().fold(*seed, |i, map| {
            for &(d, s, l) in map {
                if (s..s + l).contains(&i) {
                    return i + d - s;
                }
            }
            i
        })
    });
    let result = locs.min().unwrap();
    println!("{}", result);
}
