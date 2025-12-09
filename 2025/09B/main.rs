// Hackjob basically assumes no edge cases (abutting boundaries, etc.)
// Hope to do something more efficient and interesting later.
// Maybe represent rectilinear polys as unions of rectanlges so we can do intersection easily?
// Requires converting vertex sequence to union of rectangles. Scanline?

use std::io;

type Point = (isize, isize);

#[derive(Debug)]
struct Rectangle(Point, Point);

impl Rectangle {
    fn area(&self) -> isize {
        let (lx, ly) = self.0;
        let (rx, ry) = self.1;
        let (dx, dy) = (rx - lx, ry - ly);
        (dx.abs() + 1) * (dy.abs() + 1)
    }

    fn wholly_contains(&self, point: Point) -> bool {
        let (lx, ly) = self.0;
        let (rx, ry) = self.1;
        let minx = lx.min(rx);
        let maxx = lx.max(rx);
        let miny = ly.min(ry);
        let maxy = ly.max(ry);
        let (px, py) = point;
        minx < px && px < maxx && miny < py && py < maxy
    }

    fn segment_intersects(&self, lwr: Point, upr: Point) -> bool {
        if self.wholly_contains(lwr) {
            return true;
        }
        if self.wholly_contains(upr) {
            return true;
        }
        let (lx, ly) = self.0;
        let (rx, ry) = self.1;
        let minx = lx.min(rx);
        let maxx = lx.max(rx);
        let miny = ly.min(ry);
        let maxy = ly.max(ry);
        let (lwrx, lwry) = lwr;
        let (uprx, upry) = upr;
        let (lwrx, uprx) = (lwrx.min(uprx), lwrx.max(uprx));
        let (lwry, upry) = (lwry.min(upry), lwry.max(upry));
        let pastx: bool = uprx <= minx || maxx <= lwrx;
        let pasty: bool = upry <= miny || maxy <= lwry;
        let past: bool = pastx || pasty;
        if past {
            return false;
        }
        let spanx: bool = lwrx <= minx && maxx <= uprx;
        let spany: bool = lwry <= miny && maxy <= upry;
        spanx || spany
    }

    fn rectilinear_poly_intersects(&self, poly: &Vec<Point>) -> bool {
        let mut prev: Point = *poly.last().unwrap();
        for curr in poly {
            if self.segment_intersects(prev, *curr) {
                return true;
            }
            prev = *curr;
        }
        false
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
            let rect = Rectangle(*lhs, *rhs);
            if !rect.rectilinear_poly_intersects(&points) {
                best = best.max(rect.area());
            }
        }
    }

    println!("{}", best);
}
