#load "../utils.fs"

open _2024.utils

let areaMap = System.IO.File.ReadLines("2024/06/input") |> array2D

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
let lookUpTo view item = view |> Array.takeWhile ((<>) item)
let findFirstCrossing oldPaths newPath = ()
    
let rec moveGuard pos dir map visits =
    let view = look dir map (fst pos, snd pos)

    match view with
    | arr when Array.head arr = '#' ->
        // guard is collided with an obstacle
        moveGuard pos (rotateDirs dir) map visits
    | arr when not (Array.contains '#' arr) ->
        // guard sees no obstacles so will exit the map
        let path = [ for x in 0 .. view.Length -> (dir, move dir x pos) ]
        
        // Check for obstacle placements
        // findFirstCrossing
        
        (visits @ [path])
    | _ ->
        // guard move towards an obstacle
        let pathItems = view |> Array.takeWhile ((<>) '#')
        // let pathItems = lookUpTo view '#'
        let newPos = move dir pathItems.Length pos
        let path = [ for x in 0 .. pathItems.Length -> (dir, move dir x pos) ]
        
        moveGuard newPos dir map (visits @ [path])

// This version of the simulation does not update the map as it plays out
let simulate startPos map =
    let iInit, jInit = findGuard map
    let dirInit = getDir map[iInit, jInit]
    moveGuard startPos dirInit map [[]]
    
    // let rec moveGuardStepwise pos dir map visits =
    //     let view = look dir map (fst pos, snd pos) |> List.ofArray
    //
    //     match view with
    //     | '#' :: t ->
    //         // guard is collided with an obstacle
    //         map[fst pos, snd pos] <- '.'
    //         moveGuardStepwise pos (rotateDirs dir) map visits
    //     | [ '.' ]->
    //         // guard has reached the boundary of the map
    //         let newPos = move dir 1 pos
    //         newPos :: visits
    //     | '.' :: t ->
    //         // spawn alternate universe simulation
    //         // if alt-sim loops, log the position
    //         // let loop = spawnSim
    //         
    //         // guard moving freely
    //         let newChar = if dir = Up || dir = Do then '|' else '-'
    //         let newPos = move dir 1 pos
    //         map[fst newPos, snd newPos] <- newChar
    //         moveGuardStepwise newPos dir map (newPos :: visits)
    //     // | '-' :: t ->
    //         
    //     // | '|' :: t ->
    //     // | '+' :: t ->
    //         // guard has come across a previously traversed location
    //         // 
    //     | _ :: t -> failwith "Should not have reached this case. Other char type"
    //     | [] -> failwith "Should not have reached this case. Empty list"
    // moveGuardStepwise startPos dirInit areaMap [ startPos ]

let flatPath = simulate start areaMap |> List.concat
flatPath |> List.map snd |> Set |> Set.toList |> List.length
simulate start areaMap |> List.length




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
