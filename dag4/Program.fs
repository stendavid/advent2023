open System
open System.IO

let input = File.ReadAllLines("input")

type Card = { Winning: int array; Mine: int array }

let tryParseInt (str:string) =
    match Int32.TryParse(str) with
    | true, num -> Some num
    | _ -> None

let parseCard (input:string) =
    let parts = input.Split(':', '|')
    let wins = parts[1].Split(' ') |> Seq.choose tryParseInt |> Array.ofSeq
    let mine = parts[2].Split(' ') |> Seq.choose tryParseInt |> Array.ofSeq
    { Winning = wins; Mine = mine }

let countWinningNumbers (card: Card) =
    card.Mine
    |> Seq.where (fun n -> Seq.contains n card.Winning)
    |> Seq.length

let scoreCard (card: Card) =
    match countWinningNumbers card with
    | 0 -> 0
    | num -> 1 <<< (num-1) // shift left (power of 2)

let collectWonCards (wonCards: int array) (index,card) =
    let numWinning = countWinningNumbers card
    let duplicates = wonCards[index] + 1
    for i in index+1 .. min (index+numWinning) (wonCards.Length-1) do
        wonCards[i] <- wonCards[i] + duplicates
    wonCards

let cards = input |> Seq.map parseCard |> Array.ofSeq
let score = cards |> Seq.map scoreCard |> Seq.sum

printfn "Solution 1: %A" score

let wonCards =
    cards
    |> Seq.mapi (fun i card -> i, card)
    |> Seq.fold collectWonCards (Array.zeroCreate cards.Length)
let totalCount = (wonCards |> Seq.sum) + cards.Length;

printfn "Solution 2: %d" totalCount
