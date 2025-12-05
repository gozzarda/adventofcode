#![feature(slice_split_once)]

use std::io;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum EventKind {
    Lower,
    Query,
    Upper,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
struct Event {
    time: u64,
    kind: EventKind,
}

fn main() {
    let stdin = io::stdin();
    let lines = stdin.lines().collect::<Result<Vec<_>, _>>().unwrap();
    let (ranges, avails) = lines.split_once(|l| l.is_empty()).unwrap();
    let ranges: Vec<(u64, u64)> = ranges
        .iter()
        .map(|s| {
            let (lwr, upr) = s.split_once('-').unwrap();
            (lwr.parse().unwrap(), upr.parse().unwrap())
        })
        .collect();
    let avails: Vec<u64> = avails.iter().map(|s| s.parse().unwrap()).collect();

    let mut events: Vec<Event> = Vec::with_capacity(2 * ranges.len() + avails.len());
    for (l, u) in &ranges {
        events.push(Event {
            time: *l,
            kind: EventKind::Lower,
        });
        events.push(Event {
            time: *u,
            kind: EventKind::Upper,
        });
    }
    for t in &avails {
        events.push(Event {
            time: *t,
            kind: EventKind::Query,
        });
    }
    events.sort();

    let mut open_ranges: u64 = 0;
    let mut covered_queries: u64 = 0;
    for event in events {
        match event.kind {
            EventKind::Lower => open_ranges += 1,
            EventKind::Query => covered_queries += if open_ranges > 0 { 1 } else { 0 },
            EventKind::Upper => open_ranges -= 1,
        };
    }

    println!("{}", covered_queries);
}
