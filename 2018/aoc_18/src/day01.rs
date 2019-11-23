pub mod day01 {
    use std::collections::HashSet;

    fn read_file() -> Vec<i32> {
        std::fs::read_to_string("data/day01.in")
            .expect("Invalid file")
            .lines()
            .map(|line| line.parse::<i32>().unwrap())
            .collect()
    }

    fn solve_a(data: &Vec<i32>) -> i32 {
        data.iter().sum()
    }

    fn solve_b(data: &Vec<i32>) -> i32 {
        let mut seen = HashSet::new();
        let mut sum = 0;

        for v in data.iter().cycle() {
            sum += v;
            if !seen.insert(sum) {
                break;
            }
        }
        sum
    }

    pub fn run01() -> () {
        let data = read_file();
        let a = solve_a(&data);
        let b = solve_b(&data);
        println!("Day 01 - Part 1: {} | Part 2: {}", a, b);
    }
}
