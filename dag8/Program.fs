open System
open System.IO

let input = File.ReadAllLines("input")

type LR = { L: string; R: string }

let commands = input[0].ToCharArray() |> List.ofArray
let lrMap =
    input
    |> Seq.skip 2
    |> Seq.map (fun s -> s[0..2], { L=s[7..9]; R=s[12..14] })
    |> Map.ofSeq

let doStep cmd loc =
    lrMap[loc]
    |> (fun lr -> if cmd = 'L' then lr.L else lr.R)

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

