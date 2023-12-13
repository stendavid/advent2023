open System
open System.IO

type StoneCount =  { Green: int; Blue: int; Red: int }
type Round = StoneCount
type Game =  { ID: int; Rounds: Round array }

let get defaultValue key (map : Map<string, int>) =
    match map.TryGetValue(key) with
    | true, value -> value
    | _ -> defaultValue

let parseRound (input: string) =
    let makeColorCountPairs (a : array<string>) : array<string * int> =
        [| for i in 0..2..a.Length-1 -> a[i+1], int a[i] |]

    let counts =
        input.Split([|','; ' '|], StringSplitOptions.RemoveEmptyEntries) 
        |> makeColorCountPairs
        |> Map.ofSeq

    {
        Green = get 0 "green" counts
        Blue = get 0 "blue" counts
        Red = get 0 "red" counts
    }

let parseGame (input: string) =
    let parts = input.Split(':')
    let headerPart = parts[0]
    let roundsPart = parts[1]

    let ID = int (headerPart.Split(' ')[1])
    let rounds = roundsPart.Split(';') |> Array.map parseRound

    { ID = ID; Rounds = rounds }

let parseGames (filePath: string) =
    seq {
        use streamReader = File.OpenText(filePath)
        while not streamReader.EndOfStream do
            yield (parseGame (streamReader.ReadLine()))
    }

let gameIsValid (available : StoneCount) (game: Game) =
    let roundIsInvalid (round: Round) =
        round.Green > available.Green || round.Red > available.Red || round.Blue > available.Blue
    not (Seq.exists roundIsInvalid game.Rounds)

let green (r: Round) = r.Green
let red (r: Round) = r.Red
let blue (r: Round) = r.Blue

let getMinAvailable (g: Game) =
    {
        Red = g.Rounds |> Seq.map red |> Seq.max
        Green = g.Rounds |> Seq.map green |> Seq.max
        Blue = g.Rounds |> Seq.map blue |> Seq.max
    }

let power (count: StoneCount) = count.Red * count.Blue * count.Green

let games = parseGames "input"
let available =  { Red=12; Green=13; Blue=14 }

let sumOfIDs =
    games
    |> Seq.where (gameIsValid available)
    |> Seq.map (fun g -> g.ID)
    |> Seq.sum

printfn "Sum: %A" sumOfIDs

let sumOfPowers =
    games
    |> Seq.map (getMinAvailable >> power)
    |> Seq.sum

printfn "Power: %A" sumOfPowers
