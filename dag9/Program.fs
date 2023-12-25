open System
open System.IO

let input = File.ReadAllLines("input")

let toNumbers (s:string) = s.Split(" ") |> Array.map int

let pairwiseDiff (numbers : int array) = numbers |> Array.pairwise |> Array.map (fun (a, b) -> b-a)

let rec findNext (numbers : int array) =
    let diffs = pairwiseDiff numbers
    let delta = if Seq.forall ((=) 0) diffs then 0 else findNext diffs
    Array.last numbers + delta

let rec findPrev (numbers : int array) =
    let diffs = pairwiseDiff numbers
    let delta = if Seq.forall ((=) 0) diffs then 0 else findPrev diffs
    numbers[0] - delta

//for row in input |> Array.map toNumbers do
//     printfn "%A -> %d -- %d" row (findPrev row) (findNext row)

let sum = input |> Array.map toNumbers |> Array.map findNext |> Array.sum

printfn "Solution 1: %d" sum

let sum2 = input |> Array.map toNumbers |> Array.map findPrev |> Array.sum

printfn "Solution 2: %d" sum2
