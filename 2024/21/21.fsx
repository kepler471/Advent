#load "../utils.fs"
open Microsoft.FSharp.Collections
open _2024.utils

let numPad =
    [| "789"
       "456"
       "123"
       " 0A" |]
    |> array2D

let dirPad =
    [| " ^A"
       "<v>" |]
    |> array2D

/// A function that returns the dirs/steps required to reach a target location
/// from some source location.
///
/// e.g. for a numPad, if starting at A, heading to 5, the path could be:
///     < ^ ^
let findPath (grid: char array2d) (start: char) (goal: char) =
    let start = findChars start grid |> List.head |> Point.ofTuple 
    let goal = findChars goal grid |> List.head |> Point.ofTuple 
    goal - start
    
findPath numPad 'A' '7'
    
findPath numPad 'A' '0'
findPath numPad '0' '2'
findPath numPad '2' '9'
findPath numPad '9' 'A'
sign 0
let vec2keys ({ x = x; y = y }: Point) =
    let down, right = sign x = 1, sign y = 1
    let verChar = if down then "v" else "^"
    let horChar = if right then ">" else "<"
    if right then 
    // if (not down && right) || (down && right) then 
        repStr horChar (abs y) + repStr verChar (abs x)
        // repStr verChar (abs x) + repStr horChar (abs y)
    else 
        repStr horChar (abs y) + repStr verChar (abs x)
        // repStr verChar (abs x) + repStr horChar (abs y)
    
vec2keys { x = -2; y = -2 }
vec2keys { x = 2; y = 2 }
vec2keys { x = -2; y = 2 }
vec2keys { x = 2; y = -2 }

let mapTargetPressToKeys targetPad targetString =
    ("A" + targetString)
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> findPath targetPad a b)
    |> Seq.map vec2keys
    |> Seq.map (fun x -> x + "A") |> String.concat ""

let mapNumToDir: string -> string = mapTargetPressToKeys numPad
let mapDirToDir: string -> string = mapTargetPressToKeys dirPad
let findRequiredSequence: string -> string = (mapNumToDir >> mapDirToDir >> mapDirToDir)
    
open System.Text.RegularExpressions

let extractNumber (input: string) =
    let pattern = @"^(\d+)A$" // Matches digits followed by 'A'
    let match' = Regex.Match(input, pattern)
    if match'.Success then
        match'.Groups.[1].Value |> int |> Some
    else
        None

let complexity (code: string) =
    let humanEntry = findRequiredSequence code
    let number = extractNumber code
    number.Value * humanEntry.Length

let codes = Input.lines "2024/21/test"
codes |> Array.map (findRequiredSequence >> _.Length)
codes |> Array.map extractNumber
codes |> Array.map (complexity) |> Array.sum

codes[4] |> findRequiredSequence

codes[4] |> mapNumToDir
codes[4] |> mapNumToDir |> String.length
codes[4] |> mapNumToDir |> mapDirToDir
codes[4] |> mapNumToDir |> mapDirToDir |> String.length
codes[4] |> mapNumToDir |> mapDirToDir |> mapDirToDir
codes[4] |> mapNumToDir |> mapDirToDir |> mapDirToDir |> String.length







// let target1 = 
//     "A029A" |> Seq.pairwise |> Seq.map (fun (a, b) -> findPath numPad a b)
//     |> Seq.map vec2keys
//     |> Seq.map (fun x -> x + "A") |> String.concat ""
//     
// let target2 =
//     ("A" + target1) |> Seq.pairwise |> Seq.map (fun (a, b) -> findPath dirPad a b)
//     |> Seq.map vec2keys
//     |> Seq.map (fun x -> x + "A") |> String.concat ""
//
// let target3 =
//     ("A" + target2) |> Seq.pairwise |> Seq.map (fun (a, b) -> findPath dirPad a b)
//     |> Seq.map vec2keys
//     |> Seq.map (fun x -> x + "A") |> String.concat ""
