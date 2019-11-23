pub mod day04 {
    use std::fs;

    struct Guard {
        id: usize,
        total: usize,
        mins: [u8; 60],
    }

    impl Guard {
        fn new(id: usize) -> Guard {
            Guard {
                id: id,
                total: 0,
                mins: [0; 60],
            }
        }
    }

    fn solve() -> (usize, usize) {
        let s = fs::read_to_string("data/day04_mod.in").expect("Invalid file");
        let mut guards: Vec<Guard> = Vec::with_capacity(200);
        let mut g: Guard = Guard::new(0);
        let mut b = 0;
        let mut b_min = 0;
        let mut b_id = 0;
        let mut id = 0;
        let mut best_total = 0;
        let mut best_min = 0;
        let mut best_id = 0;
        let mut best_min_val = 0;

        for line in s.lines() {
            let sp: Vec<&str> = line.split_whitespace().collect();
            let new_id: usize = sp[0].parse().expect("Failed parsing id");

            if new_id != id {
                if g.total > best_total {
                    best_total = g.total;
                    best_id = g.id;
                    for (min, val) in g.mins.iter().enumerate() {
                        if val > &best_min_val {
                            best_min_val = *val;
                            best_min = min;
                        }
                    }
                }
                for (min, val) in g.mins.iter().enumerate() {
                    if val > &b {
                        b = *val;
                        b_min = min;
                        b_id = g.id;
                    }
                }
                guards.push(g);
                g = Guard::new(new_id);
                id = new_id;
            }

            let mut n = 1;
            while n < sp.len() {
                let start: usize = sp[n].parse().expect("Failed parsing from list");
                let end: usize = sp[n + 1].parse().expect("Failed parsing from list");

                for i in start..end {
                    g.mins[i as usize] += 1;
                    g.total += 1;
                }
                n += 2;
            }
        }
        (best_id * best_min, b_id * b_min as usize)
    }

    pub fn run04() -> () {
        let (a, b) = solve();
        println!("Day 04 - Part 1: {} | Part 2: {}", a, b);
    }
}
