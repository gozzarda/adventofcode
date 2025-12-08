#![feature(binary_heap_into_iter_sorted)]

use std::cmp::Reverse;
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

    pub fn union(&mut self, i: usize, j: usize) -> usize {
        let i = self.find(i);
        let j = self.find(j);

        if i == j {
            return self.size[i];
        }

        let (i, j) = if self.size[i] < self.size[j] {
            (j, i)
        } else {
            (i, j)
        };

        self.boss[j] = i;
        self.size[i] += self.size[j];

        self.size[i]
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
    let points: Vec<Point> = lines
        .iter()
        .map(|s| {
            let (x, yz) = s.split_once(',').unwrap();
            let (y, z) = yz.split_once(',').unwrap();
            (x.parse().unwrap(), y.parse().unwrap(), z.parse().unwrap())
        })
        .collect();

    let np = points.len();

    let mut pairs: Vec<Reverse<(i64, (usize, usize))>> = Vec::with_capacity(np * (np - 1) / 2);
    for (j, rhs) in points.iter().enumerate() {
        for (i, lhs) in points[..j].iter().enumerate() {
            let d = sqdist(*lhs, *rhs);
            pairs.push(Reverse((d, (i, j))));
        }
    }
    let pairs = BinaryHeap::from(pairs);

    let mut sets = DisjointSet::new(points.len());
    for Reverse((_, (i, j))) in pairs.into_iter_sorted() {
        if sets.union(i, j) == np {
            let (ix, _, _) = points[i];
            let (jx, _, _) = points[j];
            let result = ix * jx;
            println!("{}", result);
            return;
        }
    }
}
