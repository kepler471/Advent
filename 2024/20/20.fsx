#load "../utils.fs"

open _2024.utils

let track = Input.grid "2024/20/input"
printCharMap 0 track
let getStart map = findChars 'S' map |> List.head |> Point.ofTuple 
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