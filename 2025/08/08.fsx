#load "../utils.fs"

open _2025.utils 
open System.Collections.Generic

let parse (s: string) = s.Split(",") |> Array.map float

let junctions = Input.lines "08/test" |> Array.map parse

let euclidean (a: float array) (b: float array) =
    Array.map2 (fun x y -> (y - x) ** 2.0) a b
    |> Array.sum
    |> sqrt

let deltas = Array2D.create junctions.Length junctions.Length 0.
let adjs = Array2D.create junctions.Length junctions.Length 0.

for i in 0 .. junctions.Length - 1 do
    for j in i .. junctions.Length - 1 do
        deltas[i, j] <- euclidean junctions[i] junctions[j]
        
let sortedDeltas =
    deltas
    |> flattenWithIndices
    |> Seq.filter (fun (_, _, c) -> c > 0)
    |> Seq.sortBy (fun (_, _, c) -> c)
    
let mutable connections: int Set list = []
let mutable deltas' = sortedDeltas |> Seq.toList
let mutable (bin: (int * int * float) list) = []
let mutable (state: int Set list list) = []

let allConnected (conns: int Set list) count =
    match conns with
    | [ one ] -> one.Count = count
    | _ -> false

// Now need to be able to iterate through the distances
// and start connecting the points
// while List.length bin  connections |> Set.count = Array.length junctions do
// while List.length connections <> 1 && List.length bin < 2 do
// while List.length bin < 23 do
while not (allConnected connections junctions.Length) do
    let a, b, _ = deltas'.Head
    bin <- deltas'.Head :: bin
    deltas' <- deltas'.Tail 
    
    let containsA = connections |> List.tryFindIndex (Set.contains a)
    let containsB = connections |> List.tryFindIndex (Set.contains b)
    
    match containsA, containsB with
    | Some i, Some j when i = j -> ()
    | Some i, Some j ->
        let merged = Set.union connections[i] connections[j]
        connections <- connections |> List.removeAt (max i j) |> List.removeAt (min i j)
        connections <- merged :: connections
    | Some i, None ->
        let updated = connections[i].Add(b)
        connections <- List.removeAt i connections
        connections <- updated :: connections
    | None, Some j ->
        let updated = connections[j].Add(a)
        connections <- List.removeAt j connections
        connections <- updated :: connections
    | None, None ->
        connections <- set [a; b] :: connections
    
    state <- connections :: state

// connections |> Seq.map Set.count |> Seq.sortDescending
connections
state
let a1, a2, _ = bin[0]
junctions[a1], junctions[a2]
junctions
// 12, 10
//
// let testset = Set.empty
// let testset = testset.Add(1)
// testset
// let testset = testset.Add(2)
//
// // How to build sets or graphs out of adjacency matrix?