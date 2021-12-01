let (|IsIncrease|_|) (x, y) =
    if y > x then Some () else None

let countNumberOfIncreases: seq<int * int> -> int =
    Seq.sumBy (function | IsIncrease -> 1 | _otherwise -> 0)

let part1: int seq -> int =
    Seq.pairwise >> countNumberOfIncreases

let part2: int seq -> int =
    // Credits to @mafinar for clever tripplewise implementation
    let tripplewise =                       // [a; b; c; d]
        Seq.pairwise                        // [(a, b); (b, c); (c, d)]
        >> Seq.pairwise                     // [((a, b), (b, c))); ((b, c), (c, d))]
        >> Seq.map (function
            | (x, y), (_, z) -> x, y, z)    // [(a, b, c), (b, c, d)]

                                            // [1; 2; 3; 4; 5]
    tripplewise                             // [(1, 2, 3); (2, 3, 4); (3, 4, 5)]
    >> Seq.map (fun (x, y, z) -> x + y + z) // [6; 8; 12]
    >> part1                                // 2

let run =
    System.IO.File.ReadLines
    >> Seq.map int
    >> fun input ->
        {| Part1 = part1 input
           Part2 = part2 input |}
