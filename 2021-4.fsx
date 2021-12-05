 open System

type Cell =
    { Number: int
      Found: bool }

let initCell nr =
    { Number = nr
      Found = false }

let isFound (cell: Cell) =
    cell.Found

let isNotFound = isFound >> not

let markNumber (number: int) (board: Cell[,]) =
    let mutable marked = None

    let newBoard =
        board
        |> Array2D.mapi (fun x y cell ->
            if cell.Number = number then
                marked <- Some (x, y)
                { cell with Found = true }
            else cell)

    marked, newBoard


let checkBingo ((x, y): int * int) (board: Cell[,]) =
    board.[*, y] |> Array.forall isFound || board.[x, *] |> Array.forall isFound

let calculateScore (number: int) (board: Cell[,]): int =
    seq {
        for i in [0 .. Array2D.length1 board - 1] do
            yield i, board.[i, *]
    }
    |> Seq.sumBy (snd >> Array.filter isNotFound >> Array.sumBy (fun cell -> cell.Number))
    |> fun boardSum -> boardSum * number

type State =
    | Ongoing of Cell[,]
    | Won of {| Index: int; Number: int; Board: Cell[,] |}

let calculateWinners (numbers: int seq) (boards: Cell[,] seq) =
    Seq.fold
        (fun (state: int * State seq) (number: int) ->
            let index = fst state
            let state = snd state

            (index + 1),
            state
            |> Seq.map (fun s ->
                match s with
                | Ongoing board ->
                    match markNumber number board with
                    | None, _ ->
                        Ongoing board
                    | Some xy, board when checkBingo xy board ->
                        Won {| Index = index; Number = number; Board = board |}
                    | Some _, board ->
                        Ongoing board
                | x -> x))
        (0, (boards |> Seq.map Ongoing))
        numbers
    |> snd
    |> Seq.choose (function
        | Won x -> Some x
        | _ -> None)

let parseNumbersFromInput: string seq -> int seq =
    Seq.head
    >> fun str -> str.Split(',')
    >> Seq.map int

let parseBoardsFromInput: string seq -> seq<Cell[,]> =
    Seq.tail
    >> Seq.map (fun boardStr ->
        boardStr.Split Environment.NewLine
        |> Seq.map (fun rowStr ->
            rowStr.Split(" ", StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map (int >> initCell))
        |> Seq.filter (Seq.isEmpty >> not)
        |> array2D)

let splitByDoubleNewLine (str: string) =
    str.Split (Environment.NewLine + Environment.NewLine)

let run =
    System.IO.File.ReadAllText
    >> splitByDoubleNewLine
    >> fun chunks -> parseNumbersFromInput chunks, parseBoardsFromInput chunks
    >> fun (numbers, boards) ->
        {| Part1 = calculateWinners numbers boards
                   |> Seq.sortBy (fun x -> x.Index)
                   |> Seq.map (fun x -> calculateScore x.Number x.Board)
                   |> Seq.tryHead
           Part2 = calculateWinners numbers boards
                   |> Seq.sortByDescending (fun x -> x.Index)
                   |> Seq.map (fun x -> calculateScore x.Number x.Board)
                   |> Seq.tryHead |}