open System
open System.IO
open System.Collections.Generic
open MathNet.Numerics

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
    |> Seq.map (fun s -> s[0..2], ({ L=s[7..9]; R=s[12..14] } : LR))
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
printfn "Path: %A" (List.rev path)
printfn "Solution 1: %d" (path.Length - 1)

//--------------------------------------
// Part 2
//--------------------------------------
[<TailCall>]
let rec findLoop (loc:string) (cmds: char list) stepsAcc (indexes: Map<string,int list>) =
    let isLoop (locIndexes: Map<string, int list>) loc index =
        match locIndexes.TryFind(loc) with
        | Some list -> List.contains index list
        | _ -> false

    let addIndex (locIndexes: Map<string, int list>) loc index =
        match locIndexes.TryFind(loc) with
        | Some list -> locIndexes.Add(loc, index :: list)
        | _ ->  locIndexes.Add(loc, [ index ])

    let cmdIndex = (commands.Length - cmds.Length) % commands.Length

    let cmd, cmdsNext =
        match cmds with
        | [] -> commands.Head, commands.Tail // loop commands
        | (cmd::ctail) -> cmd, ctail

    if loc.EndsWith("Z") && isLoop indexes loc cmdIndex then
        (loc,'.',cmdIndex)::stepsAcc
    else
        let nextIndexes = if loc.EndsWith("Z") then addIndex indexes loc cmdIndex else indexes
        let locNext = doStep cmd loc
        findLoop locNext cmdsNext ((loc,cmd,cmdIndex)::stepsAcc) nextIndexes

// Find all locations ending with "A"
let startlocs = lrMap.Keys |> Seq.where (fun s -> s.EndsWith("A"))

// Collect then length from each start location to their end location 
let mutable lengths: bigint list = []

for startloc in startlocs do
    // Check if a path entry (location, command, command index) is an end location
    let isEndLoc (loc:string,_,_) = loc.EndsWith("Z")

    // Find the path from 'startloc' to an end location, continue until a cycle is found at an end location
    // (Turned out unnesseccary, but keep the code)
    printfn "Find path for %s" startloc
    let path = findLoop startloc commands [] Map.empty |> List.rev
    let endlocsWithIndex =
        path
        |> List.mapi (fun i x -> i,x)
        |> List.where (fun (_,pathentry) -> isEndLoc pathentry)

    printfn "Endpoints for %s: %A  loop = %d" startloc endlocsWithIndex (fst endlocsWithIndex[1] - fst endlocsWithIndex[0])

    // Looking for loops was unnesseccary, the path loops directly after the first end location (all end
    // locations go to the same next locations as the respective starting location)
    //
    // Length of the loop is index of the first end location
    lengths <- (bigint (path |> List.findIndex isEndLoc)) :: lengths

// Least common multiplier
let lcm a b = a * b / (bigint.GreatestCommonDivisor (a,b))
let lcmn nums = Seq.reduce lcm nums

//let lengths = [15517; 20777; 19199; 17621; 11309; 16043] |> List.map bigint

printfn "Lengths: %A" lengths

printfn "Solution 2: %A" (lcmn lengths)
