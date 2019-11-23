pub mod day05 {
    use std::fs;

    fn solve() -> (usize, usize) {
        let s = fs::read_to_string("data/day05.in").expect("Invalid file");
        let chars = s.as_bytes();
        let mut new_s = Vec::<u8>::with_capacity(s.len());

        let mut a: usize = 0;
        let mut b: usize = std::usize::MAX;

        for i in -1i32..=26 {

            let lower = if i >= 0 { b'a' + i as u8 } else { 0 };
            let upper = if i >= 0 { b'A' + i as u8 } else { 0 };

            for c in chars {
                if i >= 0 && (*c == lower || *c == upper) || *c == b'\n' {
                    continue;
                }

                let should_push = match new_s.last() {
                    None       => true,
                    Some(last) => {
                        !c.eq_ignore_ascii_case(last) 
                            || c.is_ascii_lowercase() == last.is_ascii_lowercase()
                    }
                };

                if should_push {
                    new_s.push(*c);
                } else {
                    new_s.pop();
                }
            }

            let len = new_s.len();
            if i == -1 {
                a = len;
            } else {
                b = std::cmp::min(b, len);
            }

            new_s.clear();
        }
        (a, b)
    }

    pub fn run05() -> () {
        let (a, b) = solve();
        println!("Day 05 - Part 1: {} | Part 2: {}", a, b);
    }
}
