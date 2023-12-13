open System
open System.IO

let input = File.ReadAllLines("input")

[<StructuredFormatDisplay("Range\{ {Start} .. {End} \}")>]
type Range = { Start: int64; End: int64 }
type RangeMap = { Start: int64; End: int64; Delta: int64 }

let readMap (input: string array) (name: string) =
    // Find the row "<name> map:" and read until the first empty row
    //  <name> map:
    //  dest1 source1 len1
    //  ...
    //  destN sourceN lenN
    //
    input
    |> Seq.skipWhile (fun s -> not (s.StartsWith name))
    |> Seq.skip 1
    |> Seq.takeWhile (fun s -> s.Length > 0)
    |> Seq.map (fun s ->
        let parts = s.Split ' '
        let dest = int64 parts[0]
        let src = int64 parts[1]
        let len = int64 parts[2]
        let mapRow : RangeMap = {
            Start = src
            End = src + len - 1L
            Delta = dest - src }
        mapRow)
    |> Seq.sortBy _.Start
    |> Array.ofSeq

let mapValue (maps: RangeMap array) value =
    let map = maps |> Seq.tryFind (fun r -> r.Start <= value && value <= r.End)
    match map with
    | Some m -> value + m.Delta
    | _ -> value

let seedToSoil = readMap input "seed-to-soil"
let soilToFertilizer = readMap input "soil-to-fertilizer"
let fertilizerToWater = readMap input "fertilizer-to-water"
let waterToLight = readMap input "water-to-light"
let lightToTemperature = readMap input "light-to-temperature"
let temperatureToHumidity = readMap input "temperature-to-humidity"
let humidityToLocation = readMap input "humidity-to-location"

let mergeAdjacentAndSort (ranges: Range array) =
    let mutable result : Range list = []
    let sorted = ranges |> Array.sortBy _.Start
    for r in sorted do
        if result.Length = 0 || r.Start > result.Head.End then
            // We're past the previous range: no overlap
            result <- r :: result
        else
            // Overlap found: merge "result.Head" with "r"
            result <- { Start = result.Head.Start; End = r.End } :: result.Tail
    result |> List.rev |> Array.ofList

let mapRange (maps: RangeMap array) (source: Range) =
    // This assumes that "map" is sorted increasing by Start
    let mutable curMap = 0
    let mutable pos = source.Start
    let mutable result = []

    while pos <= source.End do
        if curMap >= maps.Length then
            // "pos" is after the last map: map 1-to-1 until the end
            let endd = source.End
            result <- { Start = pos; End = endd } :: result
            pos <- endd + 1L
        else 
            let m = maps[curMap]
            if pos < m.Start then
                // "pos" is between maps: map 1-to-1 until the next map starts
                let endd = min (m.Start-1L) source.End
                result <- { Start = pos; End = endd } :: result
                pos <- endd + 1L
            elif pos <= m.End then
                // "pos" is within "m": map using "m" until it ends
                let endd = min m.End source.End
                result <- { Start = pos+m.Delta; End = endd+m.Delta } :: result
                pos <- endd + 1L
            else
                // "pos" is after "m": check the next map
                curMap <- curMap + 1
    
    result |> Array.ofList

let mapRanges (map: RangeMap array) (source: Range array) =
    source |> Array.collect (mapRange map) |> mergeAdjacentAndSort

let mapSeedToLocation =
    mapValue seedToSoil >>
    mapValue soilToFertilizer >>
    mapValue fertilizerToWater >>
    mapValue waterToLight >>
    mapValue lightToTemperature >>
    mapValue temperatureToHumidity >>
    mapValue humidityToLocation

let mapSeedToLocationRanges =
    mapRanges seedToSoil >>
    mapRanges soilToFertilizer >>
    mapRanges fertilizerToWater >>
    mapRanges waterToLight >>
    mapRanges lightToTemperature >>
    mapRanges temperatureToHumidity >>
    mapRanges humidityToLocation

let seeds = input[0].Split(' ')[1..] |> Array.map int64

let solution1 = seeds |> Array.map mapSeedToLocation |> Array.min

printfn "Solution 1: %d" solution1

let seedRanges =
    seeds
    |> Seq.chunkBySize 2
    |> Seq.map (fun chunk -> {
        Start = chunk[0]
        End = chunk[0] + chunk[1] })
    |> Array.ofSeq

// Output from mapSeedToLocationRanges is sorted by _.Start so first is lowest
let solution2 = (seedRanges |> mapSeedToLocationRanges |> Array.head).Start

printfn "Solution 2: %d" solution2
