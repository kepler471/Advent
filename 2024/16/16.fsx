#load "../utils.fs"

open _2024.utils

let maze = Input.grid "2024/16/test"
printCharMap 0 maze    
let getStart map = findChars 'S' map |> List.head |> Point.ofTuple 
let getEnd map = findChars 'E' map |> List.head |> Point.ofTuple 
let isPath (x: Point) (map: char array2d) = charAt x map = '.'

let allPaths branchCond stopCond costFn map start state  =
    let rec dfs (pos: Point) (visited: Point Set) cost state =
        let visited' = visited.Add(pos)

        let neighbours =
            dirs
            |> List.map (fun dir -> dir, move dir 1 pos)
            |> List.filter (fun (_, pos') -> branchCond pos map pos' && visited.Contains(pos') |> not)

        if neighbours.IsEmpty || stopCond pos map visited then
            // end of a path
            seq { yield ([ pos ], cost) }
        else
            neighbours
            |> Seq.collect (fun (dir, pos') ->
                let cost' = cost + costFn pos pos' state dir
                (dfs pos' visited' cost' dir)
                |> Seq.map (fun (path, cost) -> (pos :: path, cost)))

    dfs start Set.empty 0 state

let pathFilter _ map pos' = charAt pos' map = '.' || charAt pos' map = 'E'
let stopCond pos map (visited: Point Set) = charAt pos map = 'E' || visited.Contains(pos)
let turnCost _ _ dir dir' = 1 + if dir = dir' then 0 else 1000
let results = allPaths pathFilter stopCond turnCost maze (getStart maze) Ri
printfn "hello"
results
let pathsToGoal = results |> Seq.filter (fst >> Seq.contains (getEnd maze)) |> Seq.toList

let plotRoute (map: char array2d) (route: Point list) =
    let map' = Array2D.copy map
    let goal = getEnd map'
    route |> Seq.iter (setCharAt map' 'o')
    setCharAt map' 'E' goal
    printCharMap 0 map'

plotRoute maze (fst pathsToGoal[1])
snd pathsToGoal[1]
