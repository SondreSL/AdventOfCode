pub mod day02 {
    use std::fs;

    fn read_file() -> String {
        fs::read_to_string("data/day02.in").expect("Invalid file")
    }

    fn solve_a(data: &String) -> i32 {
        let mut two_count = 0;
        let mut three_count = 0;

        for line in data.lines() {
            let mut count: [i32; 26] = [0; 26];

            for c in line.chars() {
                count[(c as usize) - ('a' as usize)] += 1;
            }

            if (&count).contains(&2) {
                two_count += 1;
            }
            if (&count).contains(&3) {
                three_count += 1;
            }
        }
        two_count * three_count
    }

    fn solve_b(data: &String) -> String {
        let mut s: String = String::from("Invalid");
        for line in data.lines() {
            for inner_line in data.lines() {
                if line == inner_line {
                    continue;
                }
                let mut cs: Vec<char> = Vec::new();
                let mut equals = 0;
                for (a, b) in line.chars().zip(inner_line.chars()) {
                    if a != b {
                        equals += 1;
                        continue;
                    }
                    cs.push(a);
                }
                if equals != 1 {
                    cs.clear();
                    continue;
                }
                s = cs.into_iter().collect();
                return s;
            }
        }
        s
    }

    pub fn run02() -> () {
        let data = read_file();
        let a = solve_a(&data);
        let b = solve_b(&data);
        println!("Day 02 - Part 1: {} | Part 2: {}", a, b);
    }
}
