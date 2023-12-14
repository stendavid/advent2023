open System
open System.IO

let input = File.ReadAllLines("input")

type HandType = 
    | HighestCard = 0
    | OnePair = 1
    | TwoPair = 2
    | ThreeOfAKind = 3
    | FullHouse = 4
    | FourOfAKind = 5
    | FiveOfAKind = 6

(*
    Every hand is exactly one type. From strongest to weakest, they are:

    Five of a kind, where all five cards have the same label: AAAAA
    Four of a kind, where four cards have the same label and one card has a different label: AA8AA
    Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
    Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
    Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
    One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
    High card, where all cards' labels are distinct: 23456
*)

type CardCount = (char * int)
type CardCounts = { Cards: CardCount array; Jacks: int}

let countCards (useJack:bool) (hand:string) =
    let cards = hand.ToCharArray()
    if useJack then
        let jacks = cards |> Seq.where (fun c -> c = 'J') |> Seq.length
        let cardCounts =
            cards
            |> Seq.where (fun c -> c <> 'J')
            |> Seq.countBy id
            |> Seq.cast<CardCount>
            |> Array.ofSeq
        { Cards = cardCounts; Jacks = jacks } : CardCounts
    else
        let cardCounts =
            cards
            |> Seq.countBy id
            |> Seq.cast<CardCount>
            |> Array.ofSeq
        { Cards = cardCounts; Jacks = 0 } : CardCounts

let hasCount num = Array.exists (fun (_,n) -> n = num)
let hasCountWithJacks num (counts:CardCounts) =
    if counts.Jacks >= num then true
    else
        [0..counts.Jacks] |> Seq.exists (fun usedJacks -> hasCount (num - usedJacks) (counts.Cards))

let hasDistinctCounts num1 num2 (counts:CardCount array) =
    let found1 = Array.tryFind (fun (_,n1) -> n1 = num1) counts
    match found1 with
    | Some (card1,_) -> Array.exists (fun (card2,n2) -> card2 <> card1 && n2 = num2) counts
    | None -> false

let isFiveOfAKind = hasCountWithJacks 5
let isFourOfAKind = hasCountWithJacks 4
let isFullHouse (counts:CardCounts)  =
    if hasDistinctCounts 2 3 counts.Cards then true
    elif (hasDistinctCounts 2 2 counts.Cards) && counts.Jacks = 1 then true
    elif (hasDistinctCounts 1 2 counts.Cards) && counts.Jacks = 2 then true
    else counts.Jacks >= 3
let isThreeOfAKind = hasCountWithJacks 3
let isTwoPair (counts:CardCounts) =
    if hasDistinctCounts 2 2 counts.Cards then true
    elif hasDistinctCounts 1 2 counts.Cards && counts.Jacks = 1 then true
    else counts.Jacks >= 2
let isOnePair = hasCountWithJacks 2

let handType (useJack:bool) (hand : string) =
    let counts = countCards useJack hand
    if isFiveOfAKind counts then HandType.FiveOfAKind
    elif isFourOfAKind counts then HandType.FourOfAKind
    elif isFullHouse counts then HandType.FullHouse
    elif isThreeOfAKind counts then HandType.ThreeOfAKind
    elif isTwoPair counts then HandType.TwoPair
    elif isOnePair counts then HandType.OnePair
    else HandType.HighestCard

type Hand = { Cards: string; Type: HandType }
type Play = { Hand: Hand; Bid: int }

let parsePlay (useJack:bool) (text:string) =
    let parts = text.Split(' ')
    {
        Hand = {
            Cards = parts[0]
            Type = handType useJack parts[0]
        }
        Bid = int parts[1]
    }

let getSortKey (cardRankMap:Map<char,int>) (hand:Hand) =
    Array.append
        [| int hand.Type |]
        (hand.Cards.ToCharArray() |> Array.map (fun c -> cardRankMap[c]))

let solve (useJacks:bool) (input: string array) =
    let plays = input |> Array.map (parsePlay useJacks)

    // for play in plays do
    //     printfn "Hand %A:  %A" play.Hand.Cards play.Hand.Type

    let cardRankMap = 
        if useJacks then "J23456789TQKA".ToCharArray() |> Seq.mapi (fun i c -> c,i) |> Map.ofSeq
        else             "23456789TJQKA".ToCharArray() |> Seq.mapi (fun i c -> c,i) |> Map.ofSeq

    let playsWithValue =
        plays
        |> Seq.sortBy (fun p -> getSortKey cardRankMap p.Hand)
        |> Seq.mapi (fun i p ->
            let rank = i+1
            let value = rank * p.Bid
            (p, value, rank))

    // printfn ""
    // printfn "Sorted"
    // for (play,value,rank) in playsWithValue do
    //     printfn "Hand %A:  %-15s Rank: %4d Bid: %5d Value: %d" (play.Hand.Cards) (play.Hand.Type.ToString ()) (rank) (play.Bid) (value)

    playsWithValue |> Seq.map (fun (_,value,_) -> value) |> Seq.sum

let solveWithJacks = solve true
let solveWithoutJacks = solve false

let solution1 = solveWithoutJacks input

printfn "Solution 1: %d" solution1

let solution2 = solveWithJacks input

printfn "Solution 2: %d" solution2
