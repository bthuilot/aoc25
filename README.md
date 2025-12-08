# Advent of Code 2025 - Haskell

This repositories stores solutions in [Haskell](https://www.haskell.org/) to [Advent of Code 2025](https://adventofcode.com/2025).

For more info on the approach to each day,
read the module header comment located at the top of each day's source file 
(an index is located below)

## Building/Running

This project uses [stack](https://docs.haskellstack.org/en/stable/) to manage dependencies, buidling and execution.

*NOTE*: Before running be sure to copy your input data into the file `inputs/${DAY}.txt` where `${DAY}`
is a zero padded 2 character string of the days number. for example `05` for day 5 and `12` for day 12

```bash
stack run # or make run

# run specific day
stack run 2 # run day 2

stack run 1 5 11 # only days 1, 5 and 11
```

## Tests

To test functionaility of the project, invoke the following:

```bash
make test
# or using stack
stack test
```

## Completed Days

Below is an index to every completed day's implementation source code (containing documentation of approach) and the challenge for the day

- [Day 0](src/Days/D00.hs) : *This is a test day supposed to server a placeholder until the challenge starts*
- [Day 1](src/Days/D01.hs) : [Problem](https://adventofcode.com/2025/day/1)
- [Day 2](src/Days/D02.hs) : [Problem](https://adventofcode.com/2025/day/2)
- [Day 3](src/Days/D03.hs) : [Problem](https://adventofcode.com/2025/day/3)
- [Day 4](src/Days/D04.hs) : [Problem](https://adventofcode.com/2025/day/4)
- [Day 5](src/Days/D05.hs) : [Problem](https://adventofcode.com/2025/day/5)
- [Day 6](src/Days/D06.hs) : [Problem](https://adventofcode.com/2025/day/6)
- [Day 7](src/Days/D07.hs) : [Problem](https://adventofcode.com/2025/day/7)
- [Day 8](src/Days/D08.hs) : [Problem](https://adventofcode.com/2025/day/8)

... other days not completed yet

<!-- - [Day 9](src/Days/D09.hs) : [Problem](https://adventofcode.com/2025/day/9) -->
<!-- - [Day 10](src/Days/D10.hs) : [Problem](https://adventofcode.com/2025/day/10) -->
<!-- - [Day 11](src/Days/D11.hs) : [Problem](https://adventofcode.com/2025/day/11) -->
<!-- - [Day 12](src/Days/D12.hs) : [Problem](https://adventofcode.com/2025/day/12) -->
<!-- - [Day 13](src/Days/D13.hs) : [Problem](https://adventofcode.com/2025/day/13) -->
<!-- - [Day 14](src/Days/D14.hs) : [Problem](https://adventofcode.com/2025/day/14) -->
<!-- - [Day 15](src/Days/D15.hs) : [Problem](https://adventofcode.com/2025/day/15) -->
<!-- - [Day 16](src/Days/D16.hs) : [Problem](https://adventofcode.com/2025/day/16) -->
<!-- - [Day 17](src/Days/D17.hs) : [Problem](https://adventofcode.com/2025/day/17) -->
<!-- - [Day 18](src/Days/D18.hs) : [Problem](https://adventofcode.com/2025/day/18) -->
<!-- - [Day 19](src/Days/D19.hs) : [Problem](https://adventofcode.com/2025/day/19) -->
<!-- - [Day 20](src/Days/D20.hs) : [Problem](https://adventofcode.com/2025/day/20) -->
<!-- - [Day 21](src/Days/D21.hs) : [Problem](https://adventofcode.com/2025/day/21) -->
<!-- - [Day 22](src/Days/D22.hs) : [Problem](https://adventofcode.com/2025/day/22) -->
<!-- - [Day 22](src/Days/D23.hs) : [Problem](https://adventofcode.com/2025/day/23) -->
<!-- - [Day 22](src/Days/D24.hs) : [Problem](https://adventofcode.com/2025/day/24) -->
<!-- - [Day 22](src/Days/D25.hs) : [Problem](https://adventofcode.com/2025/day/25) -->


