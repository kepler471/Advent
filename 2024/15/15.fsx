#load "../utils.fs"
#r "nuget: Plotly.NET.Interactive, 5.0.0"
#r "nuget: FSharp.Stats"

open DynamicObj.DynObj
open FSharp.Stats
open _2024.utils
open System.Text.RegularExpressions
open Plotly.NET
open System.Collections.Generic

let arrowToDir char =
    match char with
    | '^' -> Up
    | 'v' -> Do
    | '>' -> Ri
    | _ -> Le

let paras = Input.paragraphs "2024/15/test"
let warehouse = paras[0].Split("\n") |> array2D
let moves =
    paras[1].Split("\n") |> Array.collect Array.ofSeq |> Array.map arrowToDir |> Array.toList

let interleaveColumns (array1: char[,]) (array2: char[,]) =
    // Get the dimensions of the input arrays
    let rows1, cols1 = Array2D.length1 array1, Array2D.length2 array1
    let rows2, cols2 = Array2D.length1 array2, Array2D.length2 array2

    // Ensure dimensions match for interleaving
    if rows1 <> rows2 then 
        failwith "Arrays must have the same number of rows for interleaving."

    // The number of columns in the result will be cols1 + cols2
    let result = Array2D.create rows1 (cols1 + cols2) ' '

    // Fill the result array by interleaving the columns
    for row in 0 .. rows1 - 1 do
        for col in 0 .. cols1 - 1 do
            result.[row, col * 2] <- array1.[row, col]       // Even-indexed columns from array1
            result.[row, col * 2 + 1] <- array2.[row, col]   // Odd-indexed columns from array2

    result
    
let warehouseLeft =
    warehouse |> Array2D.map (fun x ->
        match x with
        | 'O' -> '['
        | _ -> x)
    
let warehouseRight =
    warehouse |> Array2D.map (fun x ->
        match x with
        | 'O' -> ']'
        | '@' -> '.'
        | _ -> x)
    
let warehouse2 = interleaveColumns warehouseLeft warehouseRight
let robotInit = findChars '@' warehouse |> List.head |> Point.ofTuple
let robotInit2 = findChars '@' warehouse2 |> List.head |> Point.ofTuple
printCharMap warehouse2 0

let look (dir: Dirs) (arr: char array2d) (ij: Point) =
    let (i, j) = ij.x, ij.y

    match dir with
    // slicing a range on an array has inbuilt bounds checking
    | Up -> arr[ .. i, j] |> Array.rev |> Array.toList
    | Do -> arr[i .. , j] |> Array.toList
    | Le -> arr[i, .. j ] |> Array.rev |> Array.toList
    | Ri -> arr[i, j .. ] |> Array.toList


let setCharAt (map: char array2d) (char: char) (at: Point) =
    map[at.x, at.y] <- char 

let fillRegion (map: char array2d) (start: Point) (push: Dirs): Point seq =
    /// TODO: Easy to convert this to work on Left and Right
    let visited = HashSet<Point>()

    let rec step (pos: Point) =
        seq {
            if visited.Contains(pos) then
                ()
            elif charAt pos map = '#' then
                failwith "CANNOT MOVE: PATH BLOCKED BY BOX"
            else
                visited.Add(pos) |> ignore
                yield pos
                let neighbours =
                    [ push; Le; Ri ]
                    |> List.map (fun dir -> move dir 1 pos)
                    |> List.filter (fun pos' -> charAt pos' map <> '.')
                
                // TODO: If wall in neighbours, clear Set, return?
                
                for neighbour in neighbours do
                    yield! step neighbour
        }

    step start

let simulate (map: char array2d) (moves: Dirs list) : char array2d =
    let map' = map |> Array2D.copy
    let rec step (moves: Dirs list) (robo: Point) =
        match moves with
        | dir :: nextMoves ->
            match dir with
            | Le | Ri ->
                let view = look dir map' robo |> List.takeWhile ((<>) '#')
                let view = view @ ['#']
                let tail = List.tail view
                match tail with
                | '.' :: _ ->
                    let robo' = move dir 1 robo
                    setCharAt map' '.' robo
                    setCharAt map' '@' robo'
                    step nextMoves robo'
                | '#' :: _ -> 
                    step nextMoves robo
                | 'O' :: t when List.contains '.' t |> not ->
                    step nextMoves robo
                | 'O' :: t when List.contains '.' t ->
                    let freeSpace = List.findIndex ((=) '.') view
                    let robo' = move dir 1 robo
                    setCharAt map' '.' robo
                    setCharAt map' '@' robo'
                    setCharAt map' 'O' (move dir freeSpace robo)
                    step nextMoves robo'
                | _ -> failwith "ERROR: Should not reach this case"
        | [] -> ()

    step moves robotInit |> ignore
    map'

// robotInit + move Le 1 robotInit

let warehouse' = simulate warehouse moves
printCharMap warehouse 0
printCharMap warehouse' 0

let sumGPSCoord (map: char array2d) =
    let allBoxes = findChars 'O' map
    allBoxes |> List.map (fun (x, y) -> 100 * x + y) |> List.sum
sumGPSCoord warehouse'

printCharMap warehouse2 0

let testmap =
    [| "##############"
       "##..........##"
       "##...[][]...##"
       "##....[]....##"
       "##....@.....##"
       "##..........##"
       "##############" |]
    |> array2D
    
let testmap3 =
    [| "##############"
       "##......##..##"
       "##...[][]...##"
       "##....[]....##"
       "##....@[][].##"
       "##..........##"
       "##############" |]
    |> array2D
let teststart = findChars '@' testmap3 |> List.head |> Point.ofTuple

let shiftChars (map: char array2d) fill dir (points: Point seq) =
    let chars = points |> Seq.toList |> List.map (fun x -> x, charAt x map)
    let chars' = chars |> List.map (fun (x, c) -> move dir 1 x, c)
    chars |> List.iter (fun (x, _) -> setCharAt map fill x)
    chars' |> List.iter (fun (x, c) -> setCharAt map c x)

shiftChars testmap '.' Up (fillRegion testmap (move Up 1 teststart) Up)
shiftChars testmap3 '.' Ri (fillRegion testmap3 (move Ri 1 teststart) Ri)
fillRegion testmap3 (move Ri 1 teststart) Ri |> Seq.toList
printCharMap testmap 0
printCharMap testmap3 0
fillRegion testmap (move Up 1 teststart) Up |> Seq.toList
let expoints =
    fillRegion testmap (move Up 1 teststart) Up |> Seq.toList |> List.map (fun x -> x, charAt x testmap)
let newexpoints = expoints |> List.map (fun (x, c) -> move Up 1 x, c)


