#load "../utils.fs"
#r "nuget: Plotly.NET.Interactive, 5.0.0"
#r "nuget: FSharp.Stats"
#r "nuget: SkiaSharp"

open System
open DynamicObj.DynObj
open FSharp.Stats
open FSharpAux
open _2024.utils
open System.Text.RegularExpressions
open Plotly.NET
open System.Collections.Generic

let maze = Input.grid "2024/16/test"
printCharMap maze 0    
let getStart map = findChars 'S' map |> List.head |> Point.ofTuple 
let getEnd map = findChars 'E' map |> List.head |> Point.ofTuple 
let isPath (x: Point) (map: char array2d) = charAt x map = '.'

type Reindeer =
    { mutable pos: Point; mutable dir: Dirs }
    member this.turn rotation =
        let d' = rotate dirs rotation this.dir
        {pos = this.pos; dir = d'}

{pos={x=2;y=4}; dir=Do}.turn(Clockwise)

let aStar (grid: char array2d) (start: Point) (goal: Point) (heuristic: Point -> Point -> int) =
    let frontier = PriorityQueue<Point * Dirs, int>()
    frontier.Enqueue((start, Ri), 0)
    let from = Dictionary<Point, Point list>()
    let cost = Dictionary<Point, int>()
    from.Add(start, [])
    cost.Add(start, 0)
    let mutable Break = false
    
    while frontier.Count > 0 && not Break do
        let current, facing = frontier.Dequeue()
        
        if current = goal then
            printfn "REACHED THE BLOODY GOAL INNIT"
            // cost[goal] <- cost[current] + 1
            // from[goal] <- current
            // Break <- true
        else
            let options = dirs |> List.filter (fun dir -> charAt (move dir 1 current) grid = '.')
            let neighbours = options |> List.map (fun dir -> move dir 1 current)
            
            for next, dir in List.zip neighbours options do
                let turnCost = if dir = facing then 0 else 1000
                let cost' = cost[current] + 1 + turnCost
                if cost.ContainsKey(next) |> not || cost' < cost[next] then
                    cost[next] <- cost'
                    let priority = cost' + (heuristic next goal)
                    frontier.Enqueue((next, dir), priority)
                    from[next] <- [current]
    from, cost        

let fromA, costA = aStar maze (getStart maze) (getEnd maze) (manhattan)
// aStar maze (getStart maze) (getEnd maze) (manhattan) |> ignore

// printCharMap maze 0   
// fromA
// costA[getEnd maze]
// fromA[getEnd maze]
// costA[{ x = 1; y = 138 }]
// costA[{ x = 2; y = 139 }]
// costA |> Seq.toList |> List.last
// fromA |> Seq.toList |> List.last

let rec reconstructPath (current: Point) (cameFrom: Dictionary<Point, Point option>) =
    match cameFrom.[current] with
    | None -> [current]
    | Some prev -> current :: reconstructPath prev cameFrom
// reconstructPath ({ x = 1; y = 138 }) fromA |> Seq.length
// reconstructPath ({ x = 2; y = 139 }) fromA |> Seq.length


// let reconstructPath (goal: Point) (cameFrom: Dictionary<Point, Point option>) =
//     let rec loop current path =
//         match cameFrom.TryGetValue(current) with
//         | (true, Some prev) -> loop prev (current :: path)
//         | _ -> current :: path
//     loop goal []
// reconstructPath ({ x = 1; y = 138 }) fromA |> Seq.length
// reconstructPath ({ x = 2; y = 139 }) fromA |> Seq.length
    

let visualizePath (maze: char array2d) (path: Point list) =
    let mazeCopy = Array2D.copy maze
    path |> List.iter (fun p -> setCharAt mazeCopy 'o' p)
    printCharMap mazeCopy 0

let goalNeighbours =
    dirs
    |> List.map (fun dir -> move dir 1 (getEnd maze))
    |> List.filter (fun pos' -> charAt pos' maze = '.')
    
goalNeighbours |> List.map (fun x -> costA[x])

visualizePath maze (reconstructPath goalNeighbours[0] fromA)
// visualizePath maze (reconstructPath goalNeighbours[1] fromA)
reconstructPath {x=9;y=3} fromA


let reconstructAllRoutes (start: Point) (from: Dictionary<Point, Point option>) =
    let rec buildPath current acc =
        match from.TryGetValue(current) with
        | (true, Some prev) -> buildPath prev (current :: acc)
        | _ -> start :: acc

    // For all keys in the dictionary (visited points), reconstruct the paths
    from.Keys
    |> Seq.map (fun point -> buildPath point [])
    |> Seq.toList
    
let allRoutes = reconstructAllRoutes (getStart maze) fromA
// allRoutes |> List.filter (fun x -> List.last x = goalNeighbours[0]) |> List.length
allRoutes |> List.filter (List.contains {x=9;y=3}) |> List.length
allRoutes |> List.filter (List.contains {x=9;y=3}) |> List.filter (List.contains goalNeighbours[0]) |> List.length

fromA |> Seq.filter (fun x ->
    let k, v = x.Key, x.Value
    k = {x=9;y=3} )