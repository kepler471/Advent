#load "../utils.fs"

open _2024.utils

let areaMap = System.IO.File.ReadLines("2024/05/input") |> array2D

let findChars (char: char) (arr: char array2d) =
    arr
    |> Array2D.mapi (fun i j c -> if c = char then Some(i, j) else None)
    |> Seq.cast<Option<int * int>> // Converts the 2D array to a sequence of options
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.toList

let findGuard map = findChars '^' map |> List.head
let start = findGuard areaMap
let obstacles = findChars '#' areaMap

let getDir =
    function
    | '^' -> Up
    | 'v' -> Do
    | '>' -> Le
    | '<' -> Ri
    | c -> failwithf $"Invalid character: %c{c}"

// Look provides the "view" in a direction, from the perspective of a given point
// in a 2D array/matrix. The directions are labelled in natural terms, so going
// "down" means increase the i index value.
let look (dir: Dirs) (arr: 'a array2d) (ij: int * int) =
    let (i, j) = ij

    match dir with
    // slicing a range on an array has inbuilt bounds checking
    | Up -> arr[.. i - 1, j] |> Array.rev
    | Do -> arr[i + 1 .., j]
    | Le -> arr[i, .. j - 1] |> Array.rev
    | Ri -> arr[i, j + 1 ..]

printfn $"Part One: %d{0}"
printfn $"Part Two: %d{0}"

let rotateDirs = rotate dirs Clockwise


// This version of the simulation does not update the map as it plays out
let simulate startPos map =
    let rec moveGuard pos dir visits =
        let view = look dir map (fst pos, snd pos)

        match view with
        | arr when Array.head arr = '#' ->
            // guard is collided with an obstacle
            moveGuard pos (rotateDirs dir) visits
        | arr when not (Array.contains '#' arr) ->
            // guard sees no obstacles so will exit the map
            let path = [ for x in 0 .. view.Length -> move dir x pos ]
            (visits @ path)
        | _ ->
            // guard move towards an obstacle
            let pathItems = view |> Array.takeWhile ((<>) '#')
            let newPos = move dir pathItems.Length pos
            let path = [ for x in 0 .. pathItems.Length -> move dir x pos ]
            moveGuard newPos dir (visits @ path)

    let iInit, jInit = findGuard map
    let dirInit = getDir map[iInit, jInit]
    moveGuard startPos dirInit [ startPos ]

simulate start areaMap |> Set |> Set.toList |> List.length
//
// not (Array.contains '#' [|'.'|])
//
// look Up areaMap start
// |> Array.takeWhile ((<>) '#')
//
// List.map (fun x -> move x 4 (5,5)) dirs
// move Up 4 (5, 5)
// let view = look Up areaMap start
// let path = view |> Array.takeWhile ((<>) '#')
// [ for x in 0..path.Length -> move Up x start ]
// [ for x in 0..view.Length -> move Up x start ]
