# Advent of Code 2020

**Thoughts from each day**

[gauteabgit]: www.github.com/gauteab
[sarekgit]: www.github.com/sarsko

## Day 1

**[Task text][aoc1] | [Code][day1]**

[aoc1]: www.https://adventofcode.com/2020/day/1
[day1]: src/Day01.hs
[mstksg]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md#day-1

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
there is not a list in sight, but everything flows in the same direction. An
`x` is produced, then a `y` is produced, and the a check happens. If `x + y`
does not equal `2020`, then this branch is disgarded, and the next possible
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

## Day 2

**[Task text][aoc2] | [Code][day2]**

[aoc2]: www.https://adventofcode.com/2020/day/2
[day2]: src/Day02.hs

Once you have all the passwords into a single data structure, you can simply
filter out the invalid ones and count the remaining elements in the list. I
have a function `count` in my utils that will do this filtering and counting
for any foldable data structure. This function has turned out useful for days
2, 3 and 4 so far, and will probably see a lot more use throughout advent.

```haskell
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = length . filter p . toList
```

When solving part 2, it is necessary to lookup an index in a list, which is
really nice in Haskell as such lookup-functions generally return a `Maybe` if
the index is out of bounds. Comparing to indexes lets you just compare the two
`Maybe`s, without thinking about error checking.

## Day 3

**[Task text][aoc3]**

[aoc3]: www.https://adventofcode.com/2020/day/3
[day3]: src/Day03.hs
[iterlink]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:iterate

A common problem when one start to program in Haskell is how to emulate loops
without writing explicit recursion. It feels very heavy-handed to write a
recursive function when you just want to iterate some integers.  While there
are many ways to get around this, one way of solving it is to create a list the
integers you want to loop over, and then map some function over that list by
itself or after zipping it with a list of elements are working on.

In this case, we want a list of all the points on the slope that our toboggan
will need to enter in order to traverse to the bottom. I chose to
[iterate][iterlink] a function that adds the appropriate `x` and `y` coordinate
to the previous `(x, y)` coordinate, starting with `(0, 0)`. This could also be
accomplished with a recursive function, or zipping together the list of
x-coordinates with the list of y-coordinates.

```haskell
-- Recursive function
coords :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
coords (dx, dy) (x, y) = (x, y) : coords (dx, dy) (x + dx, y + dy)

-- Zipping xs and ys, assuming we start at (0, 0)
coords :: Int -> Int -> [(Int, Int)]
coords dx dy = zip [0, dx ..] [0, dy ..]
```

For this particular problem we also have to account for the input repeating to
the right. I chose to `modulo` the x-coordinate by the number of x-coordinates
given. This can either be added into the above lists directly, mapped over the
resulting lists before indexing, or while indexing. Another possible and rather
common solution is to call `cycle` on the lists given by the input. Using
Haskell's lazyness to only generate as much of the lists as necessary to
successfully return the value at the requested index is an elegant solution to
the problem.

## Day 4

**[Task text][aoc4] | [Code][day4]**

[aoc4]: www.https://adventofcode.com/2020/day/4
[day4]: src/Day04.hs
[regexlink]: https://hackage.haskell.org/package/lens-regex-pcre
[sarekreg]: https://github.com/sarsko/aoc-2020/blob/main/4/day04_2.kt
[mstksgday4]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md#day-4

The *simple* solution today is to create a regex that will look for the
appropriate patterns in each block and return all those matches to be counted.
Using Chris Penner's [lens-regex-pcre][regexlink]-library, the solution to the
second problem is:

```haskell
count ((==7) . length) $ map r input
  where r s =
      s ^.. (   [regex|byr:(19[2-9][0-9]|200[0-2])|]
             <> [regex|iyr:(201[0-9]|2020)|]
             <> [regex|eyr:(202[0-9]|2030)|]
             <> [regex|hgt:(1[5-8][0-9]|19[0-3])cm|]
             <> [regex|hgt:(5[8-9]|6[0-9]|7[0-6])in|]
             <> [regex|hcl:#[0-9a-f]{6}|]
             <> [regex|ecl:(amb|blu|brn|gry|grn|hzl|oth)|]
             <> [regex|pid:[0-9]{9}\b|]
            )
            . match
```

It is really nice that separate regexes can be fused together with `<>`, thus
avoiding [lines of 300+ characters][sarekreg]. Credit to [Sarek][sarekgit] for providing
the actual regex I used here.

A more ambitious solution is to actually parse each field into a datatype.
While such a solution was successfully crafted by [gauteab][gauteabgit] and
myself, I would suggest [Justin Le's reflections][mstksgday4] for a
comprehensive solution, utilizing some amazing libraries and showcasing the
power of algebraic data types.

It is really cool to imagine some abstract idea in your head, like a reusable
data type that can in one instance hold optional values, one instance functions
for parsing, and in the final instance only instantiated values, and then
see it actualized in code.

## Day 5

**[Task text][aoc5] | [Code][day5]**

[aoc5]: www.https://adventofcode.com/2020/day/5
[day5]: src/Day05.hs

Once you realize that the seating system is just binary numbers, mapping
each letter to either `0` or `1` pretty much solves everything. For part 1,
return the maximum value in the list.

Part 2 shows how composition can be used to build up a larger function from
many small combinators.

```haskell
part2 :: [String] -> Maybe Int
part2 =
    fmap snd
        . find (uncurry (/=))
        . (zip <*> (tail . map pred))
        . sort
        . map seat
```

Several minor steps are made to a couple of the functions used. `uncurry`
alters a function from one that takes two arguments into one that takes a tuple
of two elements instead. Thus, `find (uncurry (/=))` will return the first
tuple in the list with different elements.

The more complex `(zip <*> (tail . map pred))` combines two functions that will
be called on the same argument into a single function taking only one argument.
(`(f <*> g)` is the same as `\xs -> f xs (g xs)`). By combining all these
smaller functions, we get a single function that will find the missing number
in a sequence of sorted numbers.

## Day 6

**[Task text][aoc6] | [Code][day6]**

[aoc6]: www.https://adventofcode.com/2020/day/6
[day6]: src/Day06.hs

Very simple day, taking either the union or the intersection of the answers in
a given group and finding the length of the final set, and then summing all the
lengths.

I actually misread the question and found the answer to part 2 first, which
allowed me to submit my answers with a ~15 second gap once I had read the
question.

## Day 7

**[Task text][aoc07] | [Code][day07]**

[aoc07]: www.https://adventofcode.com/2020/day/7
[day07]: src/Day07.hs

Fun puzzle, and the first with graphs this year.

I'll show off only one aspect of haskell, namely how clean recursing a
graph can be by utilizing `Maybe`.

`findAll` will find all the vertices that can be reached from the list of
vertices in `xs`, which are the children of the vertex we are interested in. It
finds all the children of all the vertices in `xs` by calling `mapMaybe` with
`Map.lookup` over the graph. As a result, the vertices with children are
returned inside a `Just`, while those without children return `Nothing`.
`mapMaybe` then automatically discard the `Nothing`s and unpacks the `Just`s.
This way, we get all valid children, nothing crashes, and no explicit error
checking has to happen.

```haskell
findAll :: Map Bag [(Int, Bag)] -> [(Int, Bag)] -> [(Int, Bag)]
findAll input xs = xs ++ concatMap (findAll input) ys
  where
    ys = mapMaybe ((`Map.lookup` input) . snd) xs
```

## Day 8
## Day 9
## Day 10

**[Task text][aoc10] | [Code][day10]**

[aoc10]: www.https://adventofcode.com/2020/day/10
[day10]: src/Day10.hs

Really fun problem today, requiring some dynamic programming (or
mathematical savvy).

I change the input during parsing to include both `0` and the maximum of the
list plus three, as well as sorting it.

Part 1, I zip the list with its own tail and find the difference of each
resulting pair, and then use a frequency map to find the number of `1`s and
`3`. It is probably overkill to use a frequency map here, as counting the `1`
and `3` in the list is fine even if it requires two passes.

```haskell
part1 :: [Int] -> Maybe Int
part1 = adapterProduct . freqs . map (uncurry subtract) . zipWithTail
  where
    adapterProduct input = (*) <$> Map.lookup 1 input <*> Map.lookup 3 input
```

Part 2 is where it gets interesting.

My initial solution was a recursive function that first got the result back
from the recursive call on the tail, hinting at a solution using a right fold
of some sort. In the end, I found a pleasing solution using `para` from the
`recursion-schemes` packages. `para` is a right fold that at each level
provides access to both the result of the recursion on the tail of the current
sublist, as well as the sublist itself. This makes it possible to determine how
many elements of the list of results can be seen from the current position, and
the sum of those elements is the value of the current position.

It is really cool to only have to think about one level of the recursion at a
time, and in general outsourcing recursion is pretty fun. The underlying
machinery inside `Data.Functor.Foldable` takes care of translating the incoming
list into its base functor, which is why I can patternmatch on `Nil` and `Cons`
in `f`.

```haskell
part2 :: [Int] -> Maybe Integer
part2 = listToMaybe . para f
  where
    f Nil = []
    f (Cons x (xs, res)) = next : res
      where
        next = max 1 . sum . zipWith const res . takeWhile (<= x + 3) $ xs
```

The same thing can be done with a `foldr` where the function returns a tuple of
the result and the current list, but this is exactly what `para` does for us.

Another very cool way to effectively memoize results of common operations is to
construct a lazy map. [Have a look at mstksg on github][mstksgday10] for an
explanation. It seems to me an even cleaner approach to these kinds of dynamic
programming problems.

[mstksgday10]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md#day-10

## Day 11
## Day 12
## Day 13
## Day 14
## Day 15
## Day 16
## Day 17
## Day 18
## Day 19
## Day 20
## Day 21
## Day 22
## Day 23
## Day 24
## Day 25
