open System
open System.IO

let input = File.ReadAllLines("input")

type LR = { L: string; R: string }

let commands = input[0].ToCharArray() |> List.ofArray

// Parse input to build a map Location -> L/R pair with next location
// Input:
//  LRRLRLRLRLRRRRLRLR....
//  
//  GLR = (SPQ, LKJ)    <-- skip to here then build map
//  LDM = (LXK, RMT)
//  ...
let lrMap =
    input
    |> Seq.skip 2
    |> Seq.map (fun s -> s[0..2], { L=s[7..9]; R=s[12..14] } : LR)
    |> Map.ofSeq

let doStep cmd loc = if cmd = 'L' then lrMap[loc].L else lrMap[loc].R

let rec travel loc cmds stepsAcc =
    if loc = "ZZZ" then (loc,'.')::stepsAcc
    else
        match cmds with
        | [] ->
            travel loc commands stepsAcc // loop commands
        | (cmd::ctail) ->
            travel (doStep cmd loc) ctail ((loc,cmd)::stepsAcc)

let path = travel "AAA" commands []
//printfn "Path: %A" (List.rev path)
printfn "Solution 1: %d" (path.Length - 1)

