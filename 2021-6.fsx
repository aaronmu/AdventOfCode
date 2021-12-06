type State =
    { Progress: int
      Zero: int64
      One: int64
      Two: int64
      Three: int64
      Four: int64
      Five: int64
      Six: int64
      Seven: int64
      Eight: int64 }

let parse (str: string) =
    str.Split ','
    |> Seq.map int
    |> Seq.countBy id
    |> Map.ofSeq
    |> fun map ->
        { Progress = 0
          Zero  = defaultArg (map.TryFind 0) 0 |> int64
          One   = defaultArg (map.TryFind 1) 0 |> int64
          Two   = defaultArg (map.TryFind 2) 0 |> int64
          Three = defaultArg (map.TryFind 3) 0 |> int64
          Four  = defaultArg (map.TryFind 4) 0 |> int64
          Five  = defaultArg (map.TryFind 5) 0 |> int64
          Six   = defaultArg (map.TryFind 6) 0 |> int64
          Seven = defaultArg (map.TryFind 7) 0 |> int64
          Eight = defaultArg (map.TryFind 8) 0 |> int64 }

let rec evolve (days: int) (state: State) =
    if state.Progress = days then state
    else
        { state with
            Progress = state.Progress + 1
            Zero     = state.One
            One      = state.Two
            Two      = state.Three
            Three    = state.Four
            Four     = state.Five
            Five     = state.Six
            Six      = state.Zero + state.Seven
            Seven    = state.Eight
            Eight    = state.Zero } |> evolve days

let sum (state: State) =
    state.Zero + state.One + state.Two + state.Three + state.Four + state.Five + state.Six + state.Seven + state.Eight

let solve (days: int) =
    evolve days >> sum

let run =
    System.IO.File.ReadAllText
    >> parse
    >> fun input ->
        {| Part1 = solve 80 input
           Part2 = solve 256 input |}