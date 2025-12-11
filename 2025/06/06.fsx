#load "../utils.fs"

open _2025.utils 
open System.Collections.Generic

let parseRow (str: string) =
    str |> (_.Split(" ") >> Array.filter ((<>) "") >> Array.map int64)
let parseOps (str: string) =
    str |> (_.Split(" ") >> Array.filter ((<>) "") >> Array.map string)

let raw = Input.lines "06/input"
let rows =
    raw |> Array.take (Array.length raw - 1) |> Array.map parseRow
let ops =
    raw |> Array.skip (Array.length raw - 1) |> Array.head |> parseOps

// Turn into matrix
// then can take columns

let m = array2D rows

// let opMap = function
//     | "*" -> Array.reduce (*)
//     | _ -> Array.sum

let opMap (op: string) (x: int64 array) =
    match op with
    | "*" -> Array.reduce (*) x
    | _ -> Array.sum x

let cols = [| for i in 0 .. Array2D.length2 m - 1 do m[*, i] |]
Array.mapi (fun i -> opMap ops[i]) cols |> Array.sum


let grid = Input.grid "06/input"
let nRow = Array2D.length1 grid
let numGrid = grid[0 .. nRow - 2, *]
printCharMap 0 grid

let colSlices = [ for i in Array2D.length2 numGrid - 1.. -1 .. 0 do i ]

let chunkies = colSlices |> List.map (fun i -> numGrid[*, i] |> System.String |> _.Trim())

let chunker (allNums: string list) =
    let rec buildChunks acc store nums =
        match nums with
        | "" :: t -> buildChunks [] (List.map int64 acc :: store) t 
        | h :: t -> buildChunks (h :: acc) store t
        | [] -> List.map int64 acc :: store
    buildChunks [] [] allNums |> List.map List.toArray |> List.toArray

chunker chunkies
Array.mapi (fun i -> opMap ops[i]) (chunker chunkies) |> Array.sum
// |> List.map (fun i -> digitsToInt64 (numGrid[*, i] |> List.ofArray |> List.map int64))