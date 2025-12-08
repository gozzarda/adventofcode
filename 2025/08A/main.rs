#![feature(binary_heap_into_iter_sorted)]

use std::collections::binary_heap::BinaryHeap;
use std::io;

struct DisjointSet {
    boss: Vec<usize>,
    size: Vec<usize>,
}

impl DisjointSet {
    pub fn new(n: usize) -> Self {
        DisjointSet {
            boss: (0..n).collect(),
            size: vec![1; n],
        }
    }

    pub fn find(&mut self, i: usize) -> usize {
        let mut root: usize = i;
        while self.boss[root] != root {
            root = self.boss[root];
        }

        let mut curr: usize = i;
        while self.boss[curr] != root {
            let boss = self.boss[curr];
            self.boss[curr] = root;
            curr = boss;
        }

        root
    }

    pub fn union(&mut self, i: usize, j: usize) {
        let i = self.find(i);
        let j = self.find(j);

        if i == j {
            return;
        }

        let (i, j) = if self.size[i] < self.size[j] {
            (j, i)
        } else {
            (i, j)
        };

        self.boss[j] = i;
        self.size[i] += self.size[j];
    }

    pub fn roots(&self) -> Vec<usize> {
        self.boss
            .iter()
            .enumerate()
            .filter_map(|(i, b)| if i == *b { Some(*b) } else { None })
            .collect()
    }
}

type Point = (i64, i64, i64);

fn sqdist(lhs: Point, rhs: Point) -> i64 {
    let (lx, ly, lz) = lhs;
    let (rx, ry, rz) = rhs;
    let (dx, dy, dz) = (rx - lx, ry - ly, rz - lz);
    dx * dx + dy * dy + dz * dz
}

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let (num_conns, points) = lines.split_first().unwrap();
    let num_conns: usize = num_conns.parse().unwrap();
    let points: Vec<Point> = points
        .iter()
        .map(|s| {
            let (x, yz) = s.split_once(',').unwrap();
            let (y, z) = yz.split_once(',').unwrap();
            (x.parse().unwrap(), y.parse().unwrap(), z.parse().unwrap())
        })
        .collect();

    let np = points.len();

    let mut pairs: Vec<(i64, (usize, usize))> = Vec::with_capacity(np * (np - 1) / 2);
    for (j, rhs) in points.iter().enumerate() {
        for (i, lhs) in points[..j].iter().enumerate() {
            let d = sqdist(*lhs, *rhs);
            pairs.push((d, (i, j)));
        }
    }
    let (pairs, _, _) = pairs.select_nth_unstable(num_conns);

    let mut sets = DisjointSet::new(points.len());
    for (_, (i, j)) in pairs {
        sets.union(*i, *j);
    }

    let sizes: Vec<usize> = sets.roots().iter().map(|i| sets.size[*i]).collect();
    let sizes = BinaryHeap::from(sizes);

    let result: usize = sizes.into_iter_sorted().take(3).product();

    println!("{}", result);
}
