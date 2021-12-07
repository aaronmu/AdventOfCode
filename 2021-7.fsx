let part1 (input: int seq): int =
    [ Seq.min input .. Seq.max input ]
    |> Seq.map (fun target -> input |> Seq.map (fun i -> abs (target - i)))
    |> Seq.map Seq.sum
    |> Seq.min

let part2 (input :int seq): int =
    [ Seq.min input .. Seq.max input ]
    |> Seq.map (fun target ->
        input
        |> Seq.map (fun i -> abs (target - i))
        |> Seq.map (fun i -> List.sum [ 1 .. i ]))
    |> Seq.map Seq.sum
    |> Seq.min

let run =
    System.IO.File.ReadAllText
    >> fun str -> str.Split ','
    >> Seq.map int
    >> fun input ->
        {| Part1 = part1 input
           Part2 = part2 input |}