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

// forward X increases the horizontal position by X units.
// down X increases the depth by X units.
// up X decreases the depth by X units.
let ``apply command without taking aim into account`` (state: State) = function
    | Forward i -> { state with HorizontalPosition = state.HorizontalPosition + i }
    | Down i -> { state with Depth = state.Depth + i }
    | Up i -> { state with Depth = state.Depth - i }

// down X increases your aim by X units.
// up X decreases your aim by X units.
// forward X does two things:
// - It increases your horizontal position by X units.
// - It increases your depth by your aim multiplied by X.
let ``apply command and take aim into account`` (state: State) = function
    | Forward i -> { state with HorizontalPosition = state.HorizontalPosition + i
                                Depth = state.Depth + state.Aim * i }
    | Down i -> { state with Aim = state.Aim + i }
    | Up i -> { state with Aim = state.Aim - i }

// Calculate the horizontal position and depth after following the planned course (represented as a sequence of commands)
// What do you get if you multiply your final horizontal position by your final depth?
let solve (applyCommand: State -> Command -> State): Command seq -> int =
    Seq.fold applyCommand initialState >> (fun state -> state.HorizontalPosition * state.Depth)

module Parser =
    let (|StartsWith|_|) (needle: string) (haystack: string) =
        if needle.Length > haystack.Length then
            None
        elif haystack.Substring(0, needle.Length) = needle then
            Some (haystack.Substring(needle.Length))
        else
            None

    let (|Int|_|) (str: string) =
        match System.Int32.TryParse (str.Trim()) with
        | true, i -> Some i
        | _ -> None

    let parseLine (index: int) = function
        | StartsWith "forward" (Int x) -> Forward x
        | StartsWith "down" (Int x) -> Down x
        | StartsWith "up" (Int x) -> Up x
        | unknownCommand -> failwith $"Failed parsing unknown command {unknownCommand} at Line {index}."

let run =
    System.IO.File.ReadLines
    >> Seq.mapi Parser.parseLine
    >> fun input ->
        {| Part1 = solve ``apply command without taking aim into account`` input
           Part2 = solve ``apply command and take aim into account`` input |}
