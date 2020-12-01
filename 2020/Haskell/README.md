# Advent of Code 2020

## Thoughts from each day

### Day 1

*[Task][aoc1]*

[aoc1]: www.https://adventofcode.com/2020/day/1
[mstksg]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md#day-1

#### The solution

My solution is basically running a double (or triple for part 2) for-loop over
the input, looking at every pair of values, returning the product of each pair
that sums to 2020. There is only one solution due to the input being
tailor-made for the problem.

```haskell
part1 :: [Int] -> Maybe Int
part1 input =
  listToMaybe
    [ x * y
    | x <- input
    , y <- input
    , x + y == 2020
    ]

part2 :: [Int] -> Maybe Int
part2 input =
  listToMaybe
    [ x * y * z
    | x <- input
    , y <- input
    , z <- input
    , x + y + z == 2020
    ]
```

The problem with this solution is that it checks `1.` every number paired with
itself, and `2.` that it checks every pair twice, with the `x` and `y` swapped
around. A better solution would only check a given number against its possible
pairings once, and not pair it with itself. I figured I had to resort to
fiddling with indexes to make this work, but [mstksg on github][mstksg] has it
figured out using `tails`, and a further solution using sets. There will
probably be many more references to his github this advent, as I have learned a
lot of haskell by reading his previous reflections on advent of code-solution.
Many modern languages have `itertools`-like packages with a function like
`combinations` that will do this, but it turns out to be really simple in
Haskell when you combine `tails` with do-notation.

#### Do-notation vs list-comprehension, or *Using the list monad*

When trying to explain monads, the *list monad* is a popular example because
everyone knows what a list is, right? However, I find the intuition for why
lists form a monad is very different from our understanding of lists as
containers.

```haskell
part1 :: [Int] -> Maybe Int
part1 input =
  listToMaybe
    [ x * y
    | x <- input
    , y <- input
    , x + y == 2020
    ]
```

I chose to do a list-comprehension for my solution, because *I wanted a list of
all the products of pairs that gave the right solution*. This is not the same
intuition as using do-notation to produce suitable answers. On the one hand, in
the list-comprehension is clear that there is a list being constructed, but
there is a directional uglyness; the output is at the top, and the actions
producing `x` and `y` follow later on. On the other hand, in the do-notation
there is not a list in sight, but everything flows in the same direction. An `x`
is produced, then a `y` is produced, and the a check happens. If `x + y` does
not equal `2020`, then this branch is disgarded, and the next possible
value of `y` is selected. In a way, it is like a constraint solver, trying to
find values for `x` and `y` that make it all the way to the bottom.

```haskell
part1 :: [Int] -> Maybe Int
part1 input = listToMaybe $ do
    x <- input
    y <- input
    guard $ x + y == 2020
    pure $ x * y
```

The values that do make it to the bottom are put into a list using `pure`, and
then the computation might continue, depending on how many values we want to
consume, and how many values the computation is able to produce. In this case,
we only create the first value using `listToMaybe`.

I used to prefer the list-comprehension because it seemed clearer to me that it
created a list, but I am now leaning towards do-notation because it allows the
code to be written and read in a single direction.
