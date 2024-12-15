open System.Collections.Generic
open Microsoft.FSharp.Core

let test = [| "125"; "17" |]
let plutoPebbles = System.IO.File.ReadAllText("2024/11/input").Split(" ")

let fn1 (stone: string) = [| "1" |]
let fn2 (stone: string) =
    let mid = stone.Length / 2
    [| (uint64 >> string) stone[.. mid - 1]; (uint64 >> string) stone[mid ..] |]
let fn3 (stone: string) = [| 2024UL * uint64 stone |> string |]

let decideRule stone =
    match stone with
    | "0" -> fn1 stone
    | _ when stone.Length % 2 = 0 -> fn2 stone
    | _ -> fn3 stone

let blink stones=
    stones |> Array.collect decideRule

let iterN n fn =
    let rec iter fn' n' = if n' = 0 then fn' else iter (fn >> fn') (n' - 1)
    iter id n

    
iterN 1 blink test
iterN 2 blink test
iterN 3 blink test
iterN 4 blink test
iterN 5 blink test
iterN 6 blink test
iterN 6 blink test |> Seq.length
iterN 25 blink test |> Seq.length
iterN 25 blink plutoPebbles |> Seq.length
iterN 75 blink plutoPebbles

    
let rec counter (x: 'a seq) (acc: uint64)=
    match x with
    | _ when Seq.isEmpty x -> acc
    | _ -> counter (Seq.tail x) (acc + 1UL)

counter (iterN 25 blink plutoPebbles) 0UL

(iterN 11 blink plutoPebbles) |> Seq.length
Seq.empty
counter (iterN 25 blink plutoPebbles)

