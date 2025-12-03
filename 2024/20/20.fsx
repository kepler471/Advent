#load "../utils.fs"

open _2024.utils
open System.Collections.Generic

let track = Input.grid "2024/20/test"
printCharMap 0 track
let getStart map = findChars 'S' map |> List.head |> Point.ofTuple 
let getEnd map = findChars 'E' map |> List.head |> Point.ofTuple 
let branchCond pos map pos' = charAt pos' map <> '#'
let stopCond pos map = charAt pos map = 'E'
let route = Search.allPaths branchCond stopCond track (getStart track) |> Seq.head

let findShortcut pos dir =
    let pos' = move dir 1 pos
    let pos'' = move dir 2 pos
    let i = List.findIndex ((=) pos) route
    if inbounds track pos'' && charAt pos' track = '#' && charAt pos'' track <> '#' then
        let j = List.findIndex ((=) pos'') route
        if j > i then
            Some (pos, pos', pos'')
        else
            None
    else
        None

let shortcuts =
    [ for pos in route -> dirs |> List.map (findShortcut pos) ]
    |> List.collect id |> List.filter _.IsSome |> List.map _.Value

let plotRoute (map: char array2d) (route: Point list) =
    let map' = Array2D.copy map
    route |> Seq.iter (setCharAt map' 'o')
    printCharMap 0 map'

plotRoute track (List.map (fun (_, x, _) -> x) shortcuts)
let trueLength = List.length shortcuts - 1

let calcSavings (shortcut: Point * Point * Point) =
    let pos, pos', pos'' = shortcut
    let i = List.findIndex ((=) pos) route
    let j = List.findIndex ((=) pos'') route
    j - i - 2
    
shortcuts |> List.map calcSavings |> List.sort |> List.countBy id
shortcuts |> List.map calcSavings |> List.filter ((<=) 100) |> List.length

let dijkstra (grid: char array2d) (start: Point) (goal: Point)=
    let frontier = PriorityQueue<Point, int>()
    frontier.Enqueue(start, 0)
    let from = Dictionary<Point, Point option>()
    let cost = Dictionary<Point, int>()
    // from.Add(start, None)
    from[start] <- None
    cost.Add(start, 0)
    let mutable Break = false
    
    // TODO: I want to use a search
    let mutable cheatsRemaining = 20
    
    while frontier.Count > 0 && not Break do
        let current = frontier.Dequeue()
        
        if current = goal then
            printfn "REACHED THE BLOODY GOAL INNIT"
            cost[goal] <- cost[current] + 1
            from[goal] <- Some(current)
            Break <- true
        else
            let options = dirs |> List.filter (fun dir -> charAt (move dir 1 current) grid <> '#')
            let neighbours = options |> List.map (fun dir -> move dir 1 current)
            
            for next, dir in List.zip neighbours options do
                let cost' = cost[current] + 1
                if cost.ContainsKey(next) |> not || cost' <= cost[next] then
                    cost[next] <- cost'
                    let priority = cost'
                    frontier.Enqueue(next, priority)
                    from[next] <- Some(current)
    from, cost
    
let reconstructPath goal (from: Dictionary<Point, Point option>) =
    let rec backtrack current path =
        match from[current] with
        | Some parent -> backtrack parent (current :: path)
        | _ -> current :: path
    backtrack goal []    

let from, costs = dijkstra track (getStart track) (getEnd track)

reconstructPath (getEnd track) from