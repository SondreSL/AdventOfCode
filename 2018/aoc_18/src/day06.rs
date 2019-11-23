pub mod day06 {
    use std::fs;
    use std::cmp::max;

    #[derive(Clone, Copy)]
    struct Coord {
        x: isize,
        y: isize,
    }

    #[derive(Clone, Copy)]
    struct Distance {
        id: isize,
        dist: isize,
    }

    fn read_file() -> (Vec<Coord>, isize, isize) {
        let mut coords: Vec<Coord> = Vec::new();
        let mut width = 0;
        let mut height = 0;
        let s = fs::read_to_string("data/day06.in").expect("Invalid file");

        for line in s.lines() {
            let mut sp = line.split(", ");
            let x: isize = sp.next().unwrap().parse().unwrap();
            let y: isize = sp.next().unwrap().parse().unwrap();

            let coord = Coord {
                x: x as isize,
                y: y as isize,
            };

            coords.push(coord);
            width = max(width, x + 1);
            height = max(height, y + 1);
        }
        (coords, width, height)
    }

    fn solve() -> (isize, isize) {
        let (coords, width, height) = read_file();
        let mut best_area = 0;

        let default = Distance {
            id: -1,
            dist: std::isize::MAX,
        };

        let mut distances: Vec<Distance> = vec![default; (width * height) as usize];
        let mut is_finite: Vec<bool> = vec![true; coords.len()];

        for row in 0..height {
            for col in 0..width {
                let entry = &mut distances[(width * row + col) as usize];

                for (id, coord) in coords.iter().enumerate() {
                    let dist = (coord.x - col).abs() + (coord.y - row).abs();

                    if dist < entry.dist {
                        entry.id = id as isize;
                        entry.dist = dist;
                    } else if entry.dist == dist {
                        entry.id = -2;
                    }

                    if (row == 0 || row == height - 1 || col == 0 || col == width - 1)
                        && entry.id != -2 
                    {
                        let idx = entry.id as usize;
                        is_finite[idx] = false;
                    }
                }
            }
        }

        let mut counts = vec![0; coords.len()];
        for entry in &distances {
            if entry.id == -2 {
                continue;
            }
        }

        (width, height)
    }

    pub fn run06() -> () {
        let (a, b) = solve();
        println!("Day 06 - Part 1: {} | Part 2: {}", a, b);
    }
}
