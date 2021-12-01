// https://adventofcode.com/2021/day/1

// given a list of depth measurements,
// count the number of times a depth measurement increases.
let part1: int seq -> int =                 // [6; 7; 6; 7]
    Seq.pairwise                            // [(6,7); (7, 6); (6, 7)]
    >> Seq.filter (fun (x, y) -> y > x)     // [(6, 7); (6, 7)]
    >> Seq.length                           // 2

// given a list of depth measurements,
// create sums of three-measurement sliding window,
// and count the number of times the sum measurement in this sliding window increases.
let part2: int seq -> int =                 // [1; 2; 3; 2; 1; 4]
    Seq.windowed 3                          // [(1,2,3); (2,3,2); (3,2,1); (2,1,4)]
    >> Seq.map Array.sum                    // [6; 7; 6; 7]
    >> part1                                // 2

let run =
    System.IO.File.ReadLines
    >> Seq.map int
    >> fun input ->
        {| Part1 = part1 input
           Part2 = part2 input |}
