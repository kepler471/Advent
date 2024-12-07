#load "../utils.fs"

open Microsoft.FSharp.Core
open _2024.utils

let wordsearch = System.IO.File.ReadLines("2024/04/test") |> array2D

// wordsearch[2, *]
// type WordSearch =

let findChars (char: char) arr =
    arr
    |> Array2D.mapi (fun i j c -> if c = char then Some(i, j) else None)
    |> Seq.cast<Option<int * int>> // Converts the 2D array to a sequence of options
    |> Seq.filter Option.isSome // Keeps only the `Some` values
    |> Seq.map Option.get // Extracts the values from `Some`
    |> Seq.toList // Converts the sequence to a list

let flattenArray2D (arr: 'T[,]) =
    seq {
        for i in 0 .. Array2D.length1 arr - 1 do
            for j in 0 .. Array2D.length2 arr - 1 do
                yield arr.[i, j]
    }

let flattenWithIndices (arr: 'T[,]) =
    seq {
        for i in 0 .. Array2D.length1 arr - 1 do
            for j in 0 .. Array2D.length2 arr - 1 do
                yield (i, j, arr.[i, j]) // Include indices and value
    }

let findIndicesOf (char: char) (arr: char[,]) =
    flattenWithIndices arr
    |> Seq.filter (fun (_, _, value) -> value = char) // Keep only matches
    |> Seq.map (fun (i, j, _) -> (i, j)) // Extract indices
    |> Seq.toList // Convert to a list

let findXs = findIndicesOf 'X'
let findMs = findIndicesOf 'M'

findChars 'X' wordsearch

// Look provides the "view" in a direction, from the perspective of a given point
// in a 2D array/matrix. The directions are labelled in natural terms, so going
// "down" means increase the i index value.
let look (dir: Dirs8) (dist: int) (ij: int * int) (arr: char array2d) =
    let (i, j) = ij

    match dir with
    | Up -> arr[i - dist .. i, j] |> Array.rev
    | Do -> arr[i .. i + dist, j]
    | Le -> arr[i, j - dist .. j] |> Array.rev
    | Ri -> arr[i, j .. j + dist]
    | UpLe -> [| for x in 0..dist -> arr[i - x, j - x] |]
    | UpRi -> [| for x in 0..dist -> arr[i - x, j + x] |]
    | DoLe -> [| for x in 0..dist -> arr[i + x, j - x] |]
    | DoRi -> [| for x in 0..dist -> arr[i + x, j + x] |]

wordsearch
look Up 3 (4, 6) wordsearch
look Do 3 (4, 6) wordsearch
look Le 3 (4, 6) wordsearch
look Ri 3 (4, 6) wordsearch
look UpLe 3 (4, 6) wordsearch
look UpRi 3 (4, 6) wordsearch
look DoLe 3 (4, 6) wordsearch
look DoRi 3 (4, 6) wordsearch


