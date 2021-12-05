open System
open System.Collections.Generic

// takeUntil is like takeWhile except that it includes the first item that matches the predicate.
// I end up googling for this function every year :P
// https://stackoverflow.com/questions/12562327/how-to-do-seq-takewhile-one-item-in-f
let takeUntil predicate (s:seq<_>) =
  let rec loop (en:IEnumerator<_>) = seq {
    if en.MoveNext() then
      yield en.Current
      if predicate en.Current then
        yield! loop en }

  seq { use en = s.GetEnumerator()
        yield! loop en }

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
        let xstep = if x1 < x2 then 1 else -1
        let ystep = if y1 < y2 then 1 else -1

        Seq.initInfinite (function
            | 0 -> x1, y1
            | i -> (x1 + xstep * i), (y1 + ystep * i))
        |> takeUntil (fun (x, y) -> (x, y) <> (x2, y2))
        |> Seq.toList
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

let parse: string seq -> Points seq =
    let split (sep: string) (str: string) = str.Split sep

    Seq.map (split " -> ")
    >> Seq.choose (
        Array.collect (split ",")
        >> (function
            | [| x1; y1; x2; y2 |] ->
                Some ((int x1, int y1), (int x2, int y2))
            | _invalid -> None))

let run =
    IO.File.ReadLines
    >> parse
    >> fun input ->
        {| Part1 = part1 input
           Part2 = part2 input |}