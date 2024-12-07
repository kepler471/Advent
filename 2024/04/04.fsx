#load "../utils.fs"

open Microsoft.FSharp.Core
open _2024.utils

let wordsearch = System.IO.File.ReadLines("2024/04/input") |> array2D

let findChars (char: char) arr =
    arr
    |> Array2D.mapi (fun i j c -> if c = char then Some(i, j) else None)
    |> Seq.cast<Option<int * int>> // Converts the 2D array to a sequence of options
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.toList

// Look provides the "view" in a direction, from the perspective of a given point
// in a 2D array/matrix. The directions are labelled in natural terms, so going
// "down" means increase the i index value.
let look (dir: Dirs8) (dist: int) (arr: 'a array2d) (ij: int * int) =
    let (i, j) = ij

    let iLo, iHi, jLo, jHi =
        arr.GetLowerBound 0, arr.GetUpperBound 0, arr.GetLowerBound 1, arr.GetUpperBound 1

    match dir with
    // slicing a range on an array has inbuilt bounds checking
    | Up -> arr[i - dist .. i, j] |> Array.rev
    | Do -> arr[i .. i + dist, j]
    | Le -> arr[i, j - dist .. j] |> Array.rev
    | Ri -> arr[i, j .. j + dist]

    // for the diagonals, need to check bounds
    | UpLe ->
        [| for x in 0..dist do
               if i - x >= iLo && j - x >= jLo then
                   yield arr[i - x, j - x] |]
    | UpRi ->
        [| for x in 0..dist do
               if i - x >= iLo && j + x <= jHi then
                   yield arr[i - x, j + x] |]
    | DoLe ->
        [| for x in 0..dist do
               if i + x <= iHi && j - x >= jLo then
                   yield arr[i + x, j - x] |]
    | DoRi ->
        [| for x in 0..dist do
               if i + x <= iHi && j + x <= jHi then
                   yield arr[i + x, j + x] |]

let lookAround (dist: int) (arr: char array2d) (ij: int * int) =
    [ look Up dist arr ij
      look Do dist arr ij
      look Le dist arr ij
      look Ri dist arr ij
      look UpLe dist arr ij
      look UpRi dist arr ij
      look DoLe dist arr ij
      look DoRi dist arr ij ]

// Checking for the X-MAS pattern
let checkXMas (arr: char array2d) (ij: int * int) =
    let i, j = ij
    let subArr = arr[i - 1 .. i + 1, j - 1 .. j + 1]

    match subArr with
    | _ when subArr.Length < 9 -> false // remove non-square arrays
    | _ ->
        // X-MAS can only appear in 4 patterns:
        //
        //      M.M     M.S     S.S     S.M
        //      .A.     .A.     .A.     .A.
        //      S.S     M.S     M.M     S.M
        let corners = [ subArr[0, 0]; subArr[0, 2]; subArr[2, 2]; subArr[2, 0] ]

        // By taking the corners as a list, all possible patterns can be
        // written out as rotations of the same list, and so we can match
        // to find only these correct arrangements.
        match corners with
        | [ 'M'; 'M'; 'S'; 'S' ]
        | [ 'M'; 'S'; 'S'; 'M' ]
        | [ 'S'; 'S'; 'M'; 'M' ]
        | [ 'S'; 'M'; 'M'; 'S' ] -> true
        | _ -> false

let findXMASs =
    findChars 'X' wordsearch
    |> List.map (lookAround 3 wordsearch >> List.map System.String)
    |> List.concat
    |> List.filter _.Equals("XMAS")
    |> List.length

let findMASxMAS =
    findChars 'A' wordsearch |> List.filter (checkXMas wordsearch) |> List.length

printfn $"Part One: %d{findXMASs}"
printfn $"Part Two: %d{findMASxMAS}"
