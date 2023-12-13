open System
open System.IO
open MathNet.Numerics

type Race = { Time: int; Leader: int }

// Time:      7  15   30
// Distance:  9  40  200
//let input = [
//    { Time =  7; Leader =   9 }
//    { Time = 15; Leader =  40 }
//    { Time = 30; Leader = 200 }
//]

// Time:        46     85     75     82
// Distance:   208   1412   1257   1410
let input = [
    { Time = 46; Leader =  208 }
    { Time = 85; Leader = 1412 }
    { Time = 75; Leader = 1257 }
    { Time = 82; Leader = 1410 }
]

let distance raceTime chargeTime = chargeTime * (raceTime - chargeTime)

let countWinningChargeTimes raceTime leader =
    let chargeTimes = [0 .. raceTime]
    chargeTimes
    |> Seq.map (distance raceTime)
    |> Seq.where (fun d -> d > leader)
    |> Seq.length

let rec newton T L (t0:bignum) (n:int): (bignum * int)=
    // T = race time
    // L = leader distance
    // t0 = first approximation 
    // d(t) = distance given the charge time t

    // Find a zero of f(t) = d(t)-L = t(T-t) - L = 0
    // f = t(T-t) - L
    // f'(t) = T - 2t

    //  t₁ =  t₀ - f(t₀) / f'(t₀)
    let f0 = t0 * (T - t0) - L
    let df0 = T - t0 * bignum.FromInt(2)

    let t1 = t0 - f0 / df0
    let f1 = t1 * (T - t1) - L

    if abs f1 < bignum.FromDecimal(0.01M) then (t1, n)
    else newton T L t1 (n+1)

let findWinRange raceTime leader =
    let start0 = raceTime * bignum.FromIntFraction (1,4)
    let start1 = raceTime * bignum.FromIntFraction (3,4)

    let (r0, _) = newton raceTime leader start0 1
    let (r1, _) = newton raceTime leader start1 1

    let lowWinTime = ceil (bignum.ToDouble r0) |> int
    let highWinTime = floor (bignum.ToDouble r1) |> int

    highWinTime - lowWinTime + 1

let winCounts = input |> Seq.map (fun r -> countWinningChargeTimes  r.Time r.Leader)

printfn "Win counts: %A" winCounts

printfn "Solution 1: %A" (winCounts |> Seq.fold (*) 1)

// Time:        46857582
// Distance:   208141212571410

let range = findWinRange (bignum.Parse "46857582") (bignum.Parse "208141212571410")

printfn "Solution 2: %A" range
