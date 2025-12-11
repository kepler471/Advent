#load "../utils.fs"

open _2025.utils

let parse (s: string) = s.Split(",") |> Array.map int64

let corners = Input.lines "09/input" |> Array.map parse

let area (a: int64 array) (b: int64 array) =
    // Array.map2 (fun x y -> y - x) a b
    // |> Array.sum
    // |> sqrt
    (1L + abs (b[0] - a[0])) * (1L + abs (b[1] - a[1])) 

let areas = Array2D.create corners.Length corners.Length 0L
// let theater = Array2D.create 10000 10000 0
// let theater = Array2D.create 9 14 0
// let mutable (state: int array2d list) = []


for i in 0 .. corners.Length - 1 do
    for j in i + 1 .. corners.Length - 1 do
        areas[i, j] <- area corners[i] corners[j]
        // let y0 = min corners[i].[1] corners[j].[1] |> int
        // let y1 = max corners[i].[1] corners[j].[1] |> int
        // let x0 = min corners[i].[0] corners[j].[0] |> int
        // let x1 = max corners[i].[0] corners[j].[0] |> int
        // theater[y0..y1, x0..x1] <- Array2D.create (1 + y1 - y0) (1 + x1 - x0) 1
        // state <- Array2D.create (1 + y1 - y0) (1 + x1 - x0) 1 :: state
 
let corners' =
    Array.append corners [|corners[0]|] |> List.ofArray |> List.pairwise

let rectCreate (a: int64 array) (b: int64 array) =
    let y0 = min a[1] b[1] |> int
    let y1 = max a[1] b[1] |> int
    let x0 = min a[0] b[0] |> int
    let x1 = max a[0] b[0] |> int
    y0, y1, x0, x1, Array2D.create (1 + y1 - y0) (1 + x1 - x0) 1

let drawPaths (paths: (int64 array * int64 array) list) length1 length2 =
    let rec draw (paths: (int64 array * int64 array) list) (grid: int array2d) =
        match paths with
        | (a, b) :: t ->
            let y0, y1, x0, x1, rect = rectCreate a b
            grid[y0..y1, x0..x1] <- rect
            draw t grid
        | [] -> grid
        
    let grid = Array2D.create length1 length2 0
    draw paths grid
//     
// let findPaths (paths: (int64 array * int64 array) list) =
//     let rec draw acc (paths: (int64 array * int64 array) list) =
//         match paths with
//         | (a, b) :: t ->
//             let y0, y1, x0, x1, rect = rectCreate a b
//             draw (rect :: acc) t
//         | [] -> acc |> List.rev
//         
//     draw [] paths

let paths = corners' |> List.map (fun (a, b) -> rectCreate a b)
// paths |> List.map (fun (_, _, _, _, x) -> Array)

// Array2D.create 5 5 0 |> Array2D.
// min 2 4

// What is the largest rectangle?
flattenArray2D areas |> Seq.sortDescending
// How many rectangles have we created
flattenArray2D areas |> Seq.sortDescending |> Seq.length
// flattenWithIndices areas |> Seq.sortByDescending (fun (_, _, x) -> x )
// corners[1], corners[5]
// area corners[1] corners[5]
// area corners[5] corners[1]
// theater[0, 4] <- 2
// printInt64Map 2 areas
// Max coords are 98400, 98298.
// corners |> Seq.sortByDescending Array.head
// corners |> Seq.sortByDescending (Array.rev >> Array.head)
corners |> Seq.map (fun x -> x[1]) |> Seq.sortDescending
corners |> Seq.map (fun x -> x[0]) |> Seq.sortDescending

// 496 pairs defining sides.
List.length corners'
// corners

// Sparsifying/compressing - sparse space
// Get all unique coordinates for the corners
let uniqueX = corners |> Array.map Array.head |> Array.distinct |> Array.sort
let nUniqueX = uniqueX |> Array.length
let uniqueY = corners |> Array.map (Array.rev >> Array.head) |> Array.distinct |> Array.sort
let nUniqueY = uniqueY |> Array.length

// new sparse space array 
let sparseX = Array.pairwise uniqueX
let nSparseX = sparseX.Length
let sparseY = Array.pairwise uniqueY
let nSparseY = sparseY.Length
let sparse = Array2D.create nSparseY nSparseX 0

// sparse

// NOTE: 
// First coordinate is the x. Second is y.
// Array indexing is i, j -> rows, columns, so y, x.

// Find rectangles that are not simple lines
let fatRectanglesCorners =
    [ for i in 0 .. corners.Length - 1 do
          for j in i + 1 .. corners.Length - 1 do
              if corners[i].[0] <> corners[j].[0] && corners[i].[1] <> corners[j].[1] then
                  corners[i], corners[j] ]

// Find all rectangles
let rectangleCorners =
    [ for i in 0 .. corners.Length - 1 do
          for j in i + 1 .. corners.Length - 1 do
              corners[i], corners[j] ]    

// Create all rectangles - use these, already ordered coordinates
// ERROR - NOT POSSIBLE, TOO BIG
// TODO: Need a simpler version of rectCreate that gives the ordered corners
// let rectangles =
//     [ for i in 0 .. corners.Length - 1 do
//           for j in i + 1 .. corners.Length - 1 do
//               rectCreate corners[i] corners[j] ]


let compress (coords: int array) =
    let rec compress' (acc: int list list) coords' =
        match coords' with
        | one :: two :: t when two - one = 1 ->
            compress' ([ one ] :: acc) (two :: t)
        | one :: two :: t when two - one = 2 ->
            compress' ([ one + 1 ] :: [ one ] :: acc) (two :: t)
        | one :: two :: t ->
            compress' ([ [ one + 1 .. two - 1 ] ] @ [ one ] :: acc) (two :: t)
        | [] | _ -> acc |> List.rev
    compress' [] (coords |> List.ofArray)

compress (uniqueX |> Array.map int) |> List.length
compress (uniqueY |> Array.map int) |> List.length


// drawPaths corners' 10 15 |> printIntMap 0
// [ [ 6 .. -1 .. 3 ] ] @ [ 3 ]:: [ [ 9 ]; [ 9 ]; [ 9 ] ]