use std::cmp::{max, min};
use std::io;

type Seg = (i64, i64);
type Shift = (i64, i64, i64);

fn seg_inter_diff(lhs: Seg, rhs: Seg) -> (Option<Seg>, Vec<Seg>) {
    let (ll, lu) = lhs;
    let (rl, ru) = rhs;
    if lu < rl || ru < ll {
        return (None, vec![lhs]);
    }
    let (il, iu) = (max(ll, rl), min(lu, ru));
    let diff = vec![(ll, il), (iu, lu)]
        .into_iter()
        .filter(|(l, u)| l < u)
        .collect();
    (Some((il, iu)), diff)
}

fn shift_seg_rem(shift: Shift, seg: Seg) -> (Option<Seg>, Vec<Seg>) {
    let (dst, src, len) = shift;
    let (inter, diff) = seg_inter_diff(seg, (src, src + len));
    let shifted = inter.map(|(l, u)| (l + dst - src, u + dst - src));
    (shifted, diff)
}

fn shift_segs_rem(shift: Shift, segs: &[Seg]) -> (Vec<Seg>, Vec<Seg>) {
    let mut shifteds: Vec<Seg> = Vec::default();
    let mut remains: Vec<Seg> = Vec::default();
    for &seg in segs {
        let (mshifted, mut remain) = shift_seg_rem(shift, seg);
        remains.append(&mut remain);
        if let Some(shifted) = mshifted {
            shifteds.push(shifted);
        }
    }
    (shifteds, remains)
}

fn shifts_segs_rem(shifts: &[Shift], segs: &[Seg]) -> (Vec<Seg>, Vec<Seg>) {
    let mut shifteds: Vec<Seg> = Vec::default();
    let mut remains: Vec<Seg> = Vec::from(segs);
    for &shift in shifts {
        let (mut shifted, remain) = shift_segs_rem(shift, &remains);
        shifteds.append(&mut shifted);
        remains = remain;
    }
    (shifteds, remains)
}

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let paras = lines.split(|l| l.is_empty()).collect::<Vec<_>>();
    let (seedsegs_para, map_paras) = paras.split_first().unwrap();
    let seedsegs: Vec<Seg> = seedsegs_para
        .first()
        .unwrap()
        .strip_prefix("seeds: ")
        .unwrap()
        .split(" ")
        .map(|w| w.parse().unwrap())
        .collect::<Vec<_>>()
        .chunks_exact(2)
        .map(|c| (c[0], c[0] + c[1]))
        .collect();
    let maps: Vec<Vec<Shift>> = map_paras
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
    let mut locsegs = maps.iter().fold(seedsegs.clone(), |segs, map| {
        let (mut shifted, mut remain) = shifts_segs_rem(map, &segs);
        remain.append(&mut shifted);
        remain
    });
    locsegs.sort();
    let (result, _) = locsegs.iter().min().unwrap();
    println!("{:?}", result);
}
