use std::io;

type Point = (isize, isize);

struct Rectangle(Point, Point);

impl Rectangle {
    fn area(&self) -> isize {
        let (lx, ly) = self.0;
        let (rx, ry) = self.1;
        let (dx, dy) = (rx - lx, ry - ly);
        (dx.abs() + 1) * (dy.abs() + 1)
    }
}

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let points: Vec<Point> = lines
        .iter()
        .map(|s| {
            let (x, y) = s.split_once(',').unwrap();
            (x.parse().unwrap(), y.parse().unwrap())
        })
        .collect();

    let mut best: isize = 1;
    for (j, rhs) in points.iter().enumerate() {
        for lhs in points[..j].iter() {
            let area = Rectangle(*lhs, *rhs).area();
            best = best.max(area);
        }
    }

    println!("{}", best);
}
