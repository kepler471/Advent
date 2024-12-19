#load "../utils.fs"

open System.Collections.Generic
open _2024.utils

let bytes =
    Input.lines "2024/18/input"
    |> Seq.map _.Split(",")
    |> Seq.map (fun arr -> { x = int arr[1]; y = int arr[0] })
    |> Seq.toList

let createMemorySpace x y = Array2D.create y x '.'

let corruptMemory (memory: char array2d) (bytes: Point list) =
    let memory' = Array2D.copy memory
    bytes |> List.iter (setCharAt memory' '#')
    memory'

let reconstructPath goal (from: Dictionary<Point, Point option>) =
    let rec backtrack current path =
        match from[current] with
        | None -> current :: path
        | Some parent -> backtrack parent (current :: path)
    backtrack goal []

let plotRoute (map: char array2d) (route: Point list) =
    let map' = Array2D.copy map
    route |> Seq.iter (setCharAt map' 'o')
    printCharMap 0 map'

let start, goal = {x=0;y=0}, {x=70;y=70} 
let memory = createMemorySpace 71 71
let memory' = corruptMemory memory (List.take 2879 bytes)
let from, cost = Search.aStar18 memory' start goal manhattan
let finalRoute = reconstructPath goal from
plotRoute memory' finalRoute


bytes |> List.head

let findBlockage bytes memory =
    let rec binarySearch n =
        let memory' = corruptMemory memory (List.take n bytes)
        let from, _ = Search.aStar18 memory' start goal manhattan
        if from.ContainsKey goal then
            let n' = n + ((List.length bytes - n) / 2)
            printfn $"Can reach goal. N was {n}, now going to {n'}"
            binarySearch n'
        else
            printfn $"Cannot reach goal. N was {n}, now going to {n/2}"
            binarySearch (n / 2)
    binarySearch (List.length bytes)

findBlockage bytes memory

let findBlockageLoop bytes memory =
    let rec loop n =
        let memory' = corruptMemory memory (List.take n bytes)
        let from, _ = Search.aStar18 memory' start goal manhattan
        if from.ContainsKey goal then
            loop (n + 1)
        else
            n
    loop 0


let blockage = bytes |> List.take (findBlockageLoop bytes memory) |> List.last 
printfn $"Part One: Cost at goal is %A{cost[goal]}"
printfn $"Part Two: First bytes to block path is %A{blockage.y},{blockage.x}"