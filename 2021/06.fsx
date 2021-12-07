let input = 
    let raw = 
        System.IO.File.ReadLines "inputs/06.txt"
        |> Seq.head
    raw.Split [|','|] |> Seq.map (fun x -> int x)

let newborn = 8
let postnatal = 6

let age fishTimers = 
    let rec decTimers timers newTimers =
        match timers with
        | [] -> newTimers |> List.toSeq
        | 0::t -> decTimers t (newborn :: postnatal :: newTimers)
        | h::t -> decTimers t ((h - 1) :: newTimers)
    decTimers (Seq.toList fishTimers) []

let rec iterate n f =
    match n with
    | 0 -> id
    | _ -> f >> iterate (n-1) f

let iterate2 n f =
    let rec iteratetail n f acc =
        match n with
        | 0 -> id
        | _ -> iteratetail (n-1) f (f >> iterate (n-1) f)
    iteratetail n f f

let rec iterage n fishTimers =
    match n with
    | 0 -> fishTimers
    | _ -> iterage (n-1) (age fishTimers)

let flatten s = 
    seq { for i in s do
            for j in i do yield j }
// ((iterate 110) age input) |> List.length

iterage 80 [3] |> Seq.length
((iterate 80) age input) |> Seq.length

let ageMaps8 = [| for f in 0..8 do  yield iterage 8 [f] |]
let ageMaps16 = [| for f in 0..8 do  yield iterage 16 [f] |]
let ageMaps32 = [| for f in 0..8 do  yield iterage 32 [f] |]
let ageMaps64 = [| for f in 0..8 do  yield iterage 64 [f] |]
let ageMaps128 = [| for f in 0..8 do  yield iterage 128 [f] |]

// seq { for i in ageMaps128 do i }
// for timer in input

// iterage 128 input |> Seq.sum
iterage 128 input |> Seq.length

let ans = 
    input 
    |> Seq.map (fun x -> ageMaps128.[int x]) 
    |> flatten
    // |> Seq.map (fun x -> ageMaps128.[x] |> Seq.length)
    // |> Seq.sum
    // |> flatten
    // |> Seq.length
// ans |> Seq.length

let finalMults = 
    input 
    |> Seq.map (fun x -> ageMaps128.[int x]) 
    |> flatten 
    |> Seq.countBy id 

let finalEpoch = 
    finalMults
    |> Seq.map (fun (x, y) -> ageMaps128.[x] |> Seq.length)

Seq.zip (finalMults |> Seq.map snd) finalEpoch 
    |> Seq.map (fun (x, y) -> uint64 x * uint64 y)
    |> Seq.sum




// let ans2 = iterage 256 input |> List.length
// ((iterate 80) age [3;4;3;1;2]) |> List.length
// ((iterate2 80) age [3;4;3;1;2]) |> List.length

// let tick fishTimers =
//     let rec day fishes day = 
// let square n = n * n
// input |> List.length
// ((iterate 2) square) 2
