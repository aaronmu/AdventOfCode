// https://adventofcode.com/2021/day/2

type Command =
    | Forward of int
    | Down of int
    | Up of int

type State =
    { HorizontalPosition: int
      Depth: int
      Aim: int }

let initialState =
    { HorizontalPosition = 0
      Depth = 0
      Aim = 0 }

let ``increase horizontal position by`` (x: int) (state: State) =
    { state with HorizontalPosition = state.HorizontalPosition + x }

let ``increase depth by`` (x: int) (state: State) =
    { state with Depth = state.Depth + x }

let ``decrease depth by`` (x: int) (state: State) =
    { state with Depth = state.Depth - x }

let ``apply command without taking aim into account`` (state: State) = function
    | Forward x -> state |> ``increase horizontal position by`` x
    | Down x    -> state |> ``increase depth by`` x
    | Up x      -> state |> ``decrease depth by`` x

let ``increase depth by aim multiplied by`` (x: int) (state: State) =
    { state with Depth = state.Depth + state.Aim * x }

let ``increase aim by`` (x: int) (state: State) =
    { state with Aim = state.Aim + x }

let ``decrease aim by`` (x: int) (state: State) =
    { state with Aim = state.Aim - x }

let ``apply command and take aim into account`` (state: State) = function
    | Forward x -> state |> ``increase horizontal position by`` x
                         |> ``increase depth by aim multiplied by`` x
    | Down x    -> state |> ``increase aim by`` x
    | Up x      -> state |> ``decrease aim by`` x

// Calculate the horizontal position and depth after following the planned course (represented as a sequence of commands)
// What do you get if you multiply your final horizontal position by your final depth?
let solve (applyCommand: State -> Command -> State): Command seq -> int =
    Seq.fold applyCommand initialState >> (fun state -> state.HorizontalPosition * state.Depth)

module Parser =
    let (|Int|_|) (str: string) =
        match System.Int32.TryParse (str.Trim()) with
        | true, i -> Some i
        | _ -> None

    let parseLine (index: int) (str: string) =
        str.Split ' '
        |> function
            | [| "forward"; (Int x) |] -> Forward x
            | [| "down"   ; (Int x) |] -> Down x
            | [| "up"     ; (Int x) |] -> Up x
            | _ -> failwith $"Failed parsing unknown command {str} at Line {index}."

let run =
    System.IO.File.ReadLines
    >> Seq.mapi Parser.parseLine
    >> fun input ->
        {| Part1 = solve ``apply command without taking aim into account`` input
           Part2 = solve ``apply command and take aim into account`` input |}
