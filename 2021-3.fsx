 open System

[<Literal>]
let NumberOfBits = 12

let ``extract the first two items`` (arr: 'T[]): 'T option * 'T option =
    Array.tryItem 0 arr, Array.tryItem 1 arr

// [| '1'; '1'; '1' |] -> 7
let binaryCharsToInt32 (chars: char[]) =
    chars |> String.Concat |> fun str -> Convert.ToInt32(str, 2)

type BitCommonality = MostCommon | LeastCommon

let extractBit (commonality: BitCommonality) (chars: char[]): char =
    let sortBy, defaultChar =
        match commonality with
        | MostCommon  -> Array.sortByDescending, '1'
        | LeastCommon -> Array.sortBy, '0'

    chars
    |> Array.countBy id
    |> sortBy snd
    |> ``extract the first two items``
    |> function
        | Some (c, _), None -> c
        | Some (c1, i1), Some (_, i2) ->
            if i1 = i2 then defaultChar else c1
        | _ -> failwith "nope"

let part1 (arr: char[,]) =
    [0 .. NumberOfBits - 1]
    |> Seq.map (fun i ->
        let col = arr.[*, i]
        [
          col |> extractBit MostCommon  // gamma bit
          col |> extractBit LeastCommon // epsilon bit
        ])
    |> array2D
    |> fun arr ->
        let gamma   = arr.[*, 0] |> binaryCharsToInt32
        let epsilon = arr.[*, 1] |> binaryCharsToInt32
        gamma * epsilon

let array2DToSeq (arr: 'T[,]) =
    seq {
        for x in [ 0 .. (Array2D.length1 arr) - 1 ] do
            arr.[x, *]
    }

let rec searchBy (extractor: char[] -> char) (currentIndex: int) (input: char[,]): int =
    let needle = extractor input.[*, currentIndex]

    input
    |> array2DToSeq
    |> Seq.filter (fun chars -> Seq.item currentIndex chars = needle)
    |> fun remainder ->
        if Seq.length remainder = 1 then
            remainder
            |> Seq.head
            |> binaryCharsToInt32
        else
            remainder
            |> array2D
            |> searchBy extractor (currentIndex + 1)

let part2 (input: char[,]): int =
    let ``oxygen generator rating`` = searchBy (extractBit MostCommon)  0 input
    let ``CO2 scrubber rating``     = searchBy (extractBit LeastCommon) 0 input

    ``oxygen generator rating`` * ``CO2 scrubber rating``

let run =
    IO.File.ReadLines
    >> array2D
    >> fun input ->
        {| Part1 = part1 input
           Part2 = part2 input |}