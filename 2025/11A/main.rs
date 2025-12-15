use std::io::{self, BufRead};

const MAX_VERTS: usize = 26 * 26 * 26;

fn tla_to_id(tla: &str) -> Option<usize> {
    if tla.len() != 3 {
        return None;
    }
    tla.chars().try_fold(0, |acc, chr| {
        if chr.is_ascii_lowercase() {
            Some((26 * acc) + (chr as usize - 'a' as usize))
        } else {
            None
        }
    })
}

fn num_paths(adj: &[Vec<usize>], src: usize, dst: usize) -> u64 {
    let mut dpt: Vec<Option<u64>> = vec![None; MAX_VERTS];
    dpt[dst] = Some(1);
    let mut stack = vec![src];
    while let Some(u) = stack.pop() {
        if dpt[u].is_some() {
            continue;
        }
        let vs = &adj[u];
        if let Some(paths) = vs.iter().map(|&v| dpt[v]).sum() {
            dpt[u] = Some(paths);
        } else {
            stack.push(u);
            stack.extend(vs.iter().filter(|&&v| dpt[v].is_none()));
        }
    }
    dpt[src].unwrap_or(0)
}

fn main() {
    let stdin = io::stdin().lock();

    let uvss = stdin.lines().map(|s| {
        let s = s.unwrap();
        let (u, vs) = s.split_once(':').unwrap();
        let u = tla_to_id(u).unwrap();
        let vs = vs
            .trim()
            .split(' ')
            .map(tla_to_id)
            .collect::<Option<Vec<_>>>()
            .unwrap();
        (u, vs)
    });

    let mut adj: Vec<Vec<usize>> = vec![vec![]; MAX_VERTS];
    for (u, vs) in uvss {
        adj[u] = vs;
    }

    let you = tla_to_id("you").unwrap();
    let out = tla_to_id("out").unwrap();
    let result = num_paths(&adj, you, out);

    println!("{}", result);
}
