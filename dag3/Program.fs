open System
open System.IO

type Range = { Start: int; End: int }
type Numref = { Row: int; Range : Range }
type Gear = { Row: int; Col: int; Number1: Numref; Number2: Numref }

let input = File.ReadAllLines("input")

let isDigit (c:char) = Char.IsDigit(c)

let findIndex predicate (text:string) (start:int) =
    let mutable i = start
    while i < text.Length && not (predicate text[i]) do
        i <- i+1
    if i = text.Length then None else Some i

let findIndexRev predicate (text:string) (start:int) =
    let mutable i = start
    while i >= 0 && not (predicate text[i]) do
        i <- i-1
    if i < 0 then None else Some i

let findLastDigitInNumber (text:string) (pos:int) =
    let nonDigitPos = findIndex (isDigit >> not) text (pos+1)
    match nonDigitPos with
    | Some p -> p-1
    | None -> text.Length-1

let findFirstDigitInNumber (text:string) (pos:int) =
    let nonDigitPos = findIndexRev (isDigit >> not) text (pos-1)
    match nonDigitPos with
    | Some p -> p+1
    | None -> 0

let expandValue minVal maxVal pos = [max minVal (pos-1) .. min maxVal (pos+1)]
let expandRange minVal maxVal (range:Range) = [max minVal (range.Start-1) .. min maxVal (range.End+1)]

let neighboursOfPoint (input:string array) (row:int) (col:int) =
    let rows = expandValue 0 (input.Length-1) row
    let cols = expandValue 0 (input[0].Length-1) col

    Seq.allPairs rows cols |> Seq.where (fun (r,c) -> r <> row || c <> col)

let neighboursOfNumber (input:string array) (num:Numref) =
    let rows = expandValue 0 (input.Length-1) num.Row
    let cols = expandRange 0 (input[0].Length-1) num.Range

    Seq.allPairs rows cols |> Seq.where (fun (r,c) -> r <> num.Row || c < num.Range.Start || c > num.Range.End)

let isTouchingSymbol (input: string array) (num: Numref) =
    let isSymbol (row,col) = not (isDigit (input[row][col])) && input[row][col] <> '.'

    neighboursOfNumber input num |> Seq.exists isSymbol

let findAllNumbers (input: string array) =
    let nextNumberInRow (text:string) (start:int) =
        match findIndex isDigit text start with
        | Some startPos ->
            Some {
                Start = startPos
                End = findLastDigitInNumber text startPos
            }
        | None -> None

    let rec nextNumber (input: string array) (row: int) (col: int) =
        if row >= input.Length then None
        else
            match nextNumberInRow input[row] col with
            | Some range -> Some { Row = row;  Range = range }
            | None -> nextNumber input (row+1) 0

    seq {
        let mutable num = nextNumber input 0 0
        while num.IsSome do
            yield num.Value
            num <- nextNumber input num.Value.Row (num.Value.Range.End + 1)
    }

let getNumberValue (input:string array) (num:Numref) =
    int (input[num.Row].Substring(num.Range.Start, num.Range.End - num.Range.Start + 1))

let numberAt (input: string array) ((row,col):(int*int)) =
    if not (isDigit (input[row][col])) then None
    else
        Some {
            Row = row;
            Range = {
                Start = findFirstDigitInNumber input[row] col
                End = findLastDigitInNumber input[row] col
            }
        }

let findAllGears (input: string array) =
    let gearAt (input: string array) ((row, col): (int * int)) =
        let numbersTouching = neighboursOfPoint input row col
                            |> Seq.choose (numberAt input)
                            |> Seq.distinct
                            |> Array.ofSeq
        if numbersTouching.Length <> 2 then None
        else
            Some {
                Row = row
                Col = col
                Number1 = numbersTouching[0]
                Number2 = numbersTouching[1]
            }

    let rows = [0..(input.Length-1)]
    let cols = [0..(input[0].Length-1)]

    Seq.allPairs rows cols
    |> Seq.where (fun (r,c) -> input[r][c] = '*')
    |> Seq.choose (gearAt input)

//for num in (findNumbers input |> Seq.where (isPartNum input)) do
//    let row = num.Row
//    let col = num.Range.Start
//    let len = num.Range.Length
//    let isPart = isPartNum input num
//    printfn "Number at %d %d: %s isPart: %A" row col (input[row].Substring(col, len)) isPart

let solution1 = 
    findAllNumbers input
    |> Seq.where (isTouchingSymbol input)
    |> Seq.map (getNumberValue input)
    |> Seq.sum

printfn "Solution 1: %d" solution1

//for g in (findGears input) do
//    printfn "Gear at %d %d, numbers=[%d %d]"
//        g.Row g.Col (getNumber input g.Number1) (getNumber input g.Number2)

let solution2 =
    findAllGears input
    |> Seq.map (fun g -> (getNumberValue input g.Number1)*(getNumberValue input g.Number2))
    |> Seq.sum

printfn "Solution 2: %d" solution2
