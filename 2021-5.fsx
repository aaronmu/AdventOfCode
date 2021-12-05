open System
open System.Text.RegularExpressions

let (|MatchAll|_|) (pattern: string) (input: string) =
    Regex.Matches (input, pattern) |> Array.ofSeq |> Array.map (fun x -> x.Value) |> Some

type Points = (int * int) * (int * int)

let ``straight line`` ((x1, y1), (x2, y2)) =
    if x1 = x2 then
        [ for y in (min y1 y2) .. (max y1 y2) do x1, y ]
    elif y1 = y2 then
        [ for x in (min x1 x2) .. (max x1 x2) do x, y2 ]
    else
        []

let ``45 degree diagonal line`` (((x1, y1), (x2, y2)): (int * int) * (int * int)) =
    if  abs (x1 - x2) = abs (y1 - y2) then
        List.zip
            [ (min x1 x2) .. (max x1 x2) ]
            [ (min y1 y2) .. (max y1 y2) ]
    else
        []

let ``straight or diagonal line`` x =
    [ yield! ``straight line`` x
      yield! ``45 degree diagonal line`` x ]

let ``collect points``: (int * int) seq -> int =
   Seq.countBy id
   >> Seq.filter (fun (_, cnt) -> cnt >= 2)
   >> Seq.length

let part1: seq<Points> -> int =
   Seq.collect ``straight line`` >> ``collect points``

let part2: seq<Points> -> int =
    Seq.collect ``straight or diagonal line`` >> ``collect points``

let parseSingleLine = function
    | MatchAll "\d+" [| x1; y1; x2; y2 |] -> (int x1, int y1), (int x2, int y2)
    | invalidLine -> failwith $"Failed to parse {invalidLine}"

let run =
    IO.File.ReadLines
    >> Seq.map parseSingleLine
    >> fun input ->
        {| Part1 = part1 input
           Part2 = part2 input |}