#load "../utils.fs"

open _2025.utils 
open System.Collections.Generic

let parse (s: string) = s.Split(",") |> Array.map float

let junctions = Input.lines "08/input" |> Array.map parse

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

// Now need to be able to iterate through the distances
// and start connecting the points
let mutable connections: int Set list = []

// Build adjacency matrix
sortedDeltas
|> Seq.takeWhile (fun _ -> List.length connections = 1)
|> Seq.iter (fun (a, b, c) ->
    adjs[a, b] <- 1
    
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
    )

connections |> Seq.map Set.count |> Seq.sortDescending
// adjs

//
// let testset = Set.empty
// let testset = testset.Add(1)
// testset
// let testset = testset.Add(2)
//
// // How to build sets or graphs out of adjacency matrix?