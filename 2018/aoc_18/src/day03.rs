pub mod day03 {
    use std::fs;
    use regex::Regex;
    use std::collections::HashMap;

    #[derive(Copy, Clone, Hash, Eq, PartialEq)]
    struct Rect {
        id: u16,
        min_x: u16,
        min_y: u16,
        max_x: u16,
        max_y: u16,
    }

    impl Rect {
        fn new(id: u16, x: u16, y: u16, width: u16, height: u16) -> Rect {
            Rect {
                id: id,
                min_x: x,
                min_y: y,
                max_x: x + width - 1,
                max_y: y + height - 1,
            }
        }
    }


    fn read_file() -> Vec<Rect> {
        let s = fs::read_to_string("data/day03.in").expect("Invalid file");
        let mut rects: Vec<Rect> = Vec::with_capacity(1300);

        let re = Regex::new(r"#(?P<id>\d+) @ (?P<x>\d+),(?P<y>\d+): (?P<width>\d+)x(?P<height>\d+)")
            .unwrap();

        for line in s.lines() {
            let data = re.captures(line).unwrap();
            let id = data["id"].parse().unwrap();
            let x = data["x"].parse().unwrap();
            let y = data["y"].parse().unwrap();
            let width = data["width"].parse().unwrap();
            let height = data["height"].parse().unwrap();

            let new_rect = Rect::new(id, x, y, width, height);
            rects.push(new_rect);
        }
        rects
    }

    fn solve(data: &Vec<Rect>) -> (u32, u32) {
        let mut map: HashMap<(u16, u16), u8> = HashMap::new();
        let mut total: u32 = 0;
        let mut non_overlap: u32 = 0;

        for r in data {
            for x in r.min_x..=r.max_x {
                for y in r.min_y..=r.max_y {
                    let val = map.entry((x, y)).or_insert(0);
                    *val += 1;
                    if val == &2 {
                        total += 1;
                    }
                }
            }
        }


        'inner: for r in data {
            for x in r.min_x..=r.max_x {
                for y in r.min_y..=r.max_y {
                    match map.get(&(x, y)) {
                        Some(x) => if x > &1 {
                            continue 'inner;
                        }
                        None    => continue,
                    }
                }
            }
            non_overlap = r.id as u32;
        };
        return (total, non_overlap);
    }

    pub fn run03() -> () {
        let data = read_file();
        let (a, b) = solve(&data);
        println!("Day 03 - Part 1: {} | Part 2: {}", a, b);
    }
}
