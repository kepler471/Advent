open Microsoft.FSharp.Core

let testPebbles =
    [| "125"; "17" |] |> Seq.countBy id |> Seq.map (fun (s, n) -> s, uint64 n)
let plutoPebbles =
    System.IO.File.ReadAllText("2024/11/input").Split(" ")
    |> Seq.countBy id
    |> Seq.map (fun (s, n) -> s, uint64 n)

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

let iterN n fn =
    let rec iter fn' n' = if n' = 0 then fn' else iter (fn >> fn') (n' - 1)
    iter id n

let blink (pebbles: (string * uint64) seq)=
    pebbles
    |> Seq.map (fun (str, n) -> decideRule str |> Array.map (fun str -> str, n))
    |> Seq.collect id
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> k, v |> Seq.sumBy snd)

iterN 25 blink plutoPebbles |> Seq.sumBy snd
iterN 75 blink plutoPebbles |> Seq.sumBy snd