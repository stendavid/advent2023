open System
open System.IO

type Map = char array array

let input = File.ReadAllLines("input")

let map : Map = input |> Array.map (fun s -> s.ToCharArray())

let findRowCol pred arr2d =
    arr2d
    |> Seq.mapi (fun row arr ->
        Seq.tryFindIndex pred arr |> Option.map (fun col -> (row, col)))
    |> Seq.choose id
    |> Seq.item 0

let inBounds (map: Map) (row,col) =
    let width = map[0].Length
    let height = map.Length 
    0 <= row && row < height && 0 <= col && col < width

let neighbours (map: Map) (row,col) =
    [ (row-1, col); (row, col-1); (row, col+1) ; (row+1, col) ] |> List.where (inBounds map)

let getTile (map: Map) (row,col) = map[row][col]

let tileConnections (tileChar: char) (row,col) =
    match tileChar with
    | '|' -> [ (row-1,col) ; (row+1, col) ]
    | '-' -> [ (row,col-1) ; (row, col+1) ]
    | 'L' -> [ (row-1,col) ; (row, col+1) ]
    | 'J' -> [ (row-1,col) ; (row, col-1) ]
    | 'F' -> [ (row,col+1) ; (row+1, col) ]
    | '7' -> [ (row+1,col) ; (row, col-1) ]
    | 'S' -> [ (row-1, col); (row, col-1); (row, col+1) ; (row+1, col) ]
    | _ -> []

let connections (map: Map) point = tileConnections (getTile map point) point

let connectsTo (map: Map) point1 point2 =
    connections map point2 |> List.contains point1 &&
        connections map point1 |> List.contains point2

let findConnectingPipes (map: Map) point =
    neighbours map point
    |> List.where (connectsTo map point)

let findNextPipe (map: Map) current previous =
    neighbours map current
    |> List.where ((<>) previous)
    |> List.where (connectsTo map current)
    |> Seq.item 0

// let followPipe (map: Map) start =
//     let rec walk point last pathAcc =
//         let next = findNextPipe map point last
//         //printfn "Walk %A %A to %A %A" point (getTile map point) next (getTile map next)
//         if next = start then
//             (point :: pathAcc)
//         else
//             walk next point (point :: pathAcc)
//     let next = findConnectingPipes map start |> Seq.item 0
//     walk next start [start] |> List.rev

let followPipe (map: Map) start =
    [
        yield start
        let mutable (prev, current) = start, findConnectingPipes map start |> Seq.item 0
        while current <> start do
             yield current
             let tmp = current
             current <- findNextPipe map current prev
             prev <- tmp
    ]

let start = findRowCol ((=) 'S') map

printfn "Start: %A" start
// printfn "Connecting pipes"
// for p in findConnectingPipes map start do
//     let next = findNextPipe map p start
//     printfn "%A %A -> %A -> %A %A" p (getTile map p) (connections map p) next (getTile map next)

let path = followPipe map start

// printfn "Path: "
// for p in path do
//     printfn "%A %A" p (getTile map p)    

printfn "Solution 1: %A" (path.Length / 2)