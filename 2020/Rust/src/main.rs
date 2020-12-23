fn main() {
    // const N: u32 = 10_000_000;
    const N: u32 = 10_000_000;
    const BIGGEST: u32 = 1_000_000;

    // Initialize everything
    // let mut fst: Vec<u32> = vec![3, 8, 9, 1, 2, 5, 4, 6, 7];
    let mut fst: Vec<u32> = vec![3, 2, 7, 4, 6, 5, 1, 8, 9];

    let mut snd: Vec<u32> = Vec::with_capacity(BIGGEST as usize);
    for _i in 0..BIGGEST as usize {
        snd.push(0);
    }

    for i in 10..=BIGGEST as usize {
        fst.push(i as u32);
    }

    for _i in 0..N {
        if _i % 20000 == 0 {
            println!("{}", _i);
        };
        let a = fst[0];
        let rest = &fst[4..];
        let destination = {
            let mut dest = if a == 1 { BIGGEST } else { a - 1 };
            loop {
                if dest == fst[1] || dest == fst[2] || dest == fst[3] {
                    if dest == 1 {
                        dest = BIGGEST;
                    } else {
                        dest -= 1;
                    }
                } else {
                    break;
                }
            }
            dest
        };

        let mut i = 0;
        let mut snd_i = 0;

        // Insert all the numbers up to the destination cup
        while i < rest.len() && rest[i] != destination {
            snd[snd_i] = rest[i];
            i += 1;
            snd_i += 1;
        }

        // Insert the picked out numbers
        snd[snd_i] = rest[i];
        snd_i += 1;
        i += 1;
        snd[snd_i] = fst[1];
        snd_i += 1;
        snd[snd_i] = fst[2];
        snd_i += 1;
        snd[snd_i] = fst[3];
        snd_i += 1;

        // Insert the rest of the numbers
        while snd_i < (BIGGEST - 1) as usize {
            snd[snd_i] = rest[i];
            i += 1;
            snd_i += 1;
        }

        assert!(snd_i == (BIGGEST - 1) as usize);

        snd[snd_i] = fst[0];
        // Swap fst and snd for next iteration
        std::mem::swap(&mut fst, &mut snd);
    }

    let mut one = 0;
    while fst[one] != 1 {
        one += 1;
    }
    println!("{}", fst[(one + 1) % BIGGEST as usize]);
    println!("{}", fst[(one + 2) % BIGGEST as usize]);
}
