#load "../utils.fs"

open _2024.utils

let map = System.IO.File.ReadAllLines("2024/10/test") |> array2D

let trailheads = findChars '0' map

let isStepSize n a b = abs (fst a - fst b) + abs (snd a - snd b) <= n
let isGradualStep a b = b - a = 1 
let charFromPos (pos: int * int) (arr: char array2d) : int = digitToInt arr[fst pos, snd pos]  
let inbounds (map: char array2d) pos =
    fst pos >= map.GetLowerBound 0 && fst pos <= map.GetUpperBound 0 &&
    snd pos >= map.GetLowerBound 0 && snd pos <= map.GetUpperBound 0

let traverse (map: char array2d) (start: int * int)=
    let visited = System.Collections.Generic.HashSet<(int * int)>()

    let rec loop pos = seq {
        if visited.Contains(pos) then
            ()
        else
            visited.Add(pos) |> ignore
            let height = charFromPos pos map
            yield (pos, height)

            if height < 9 then
                let neighbours =
                    dirs
                    |> List.map (fun dir -> move dir 1 pos)
                    |> List.filter (fun pos' ->
                        inbounds map pos' && isGradualStep height (charFromPos pos' map))

                for neighbor in neighbours do
                    yield! loop neighbor
    }

    loop start
    
let calculateTrailheadScore (map: char array2d) (start: int * int) =
    traverse map start
    |> Seq.filter (fun (_, value) -> value = 9)
    |> Seq.length    

trailheads |> List.map (calculateTrailheadScore map) |> List.sum

trailheads[1] |> (traverse map) |> Seq.toList
trailheads.Head |> (traverse map) |> Seq.toList
isGradualStep (charFromPos (0, 2) map) (charFromPos (0, 3) map)
(charFromPos (0, 3) map) - (charFromPos (0, 2) map) = 1
inbounds map (0, 0 )
map
trailheads |> List.map (traverseMap map) |> Seq.map Seq.toList

let findPeaks start (map: char array2d) =
    let visited = System.Collections.Generic.HashSet<(int * int)>()
    let rec search (pos: int * int) (map: char array2d) acc =
        if map[fst pos, snd pos] = '9' then pos :: acc
        else
        // let options =
        dirs
        |> List.filter (fun dir ->
            let newPos = move dir 1 pos
            isStepSize 1 pos newPos && inbounds newPos map)
        |> List.fold (fun acc dir -> search (move dir 1 pos) map acc)
    search start map []

findPeaks trailheads.Head map

let findPeaks (start: int * int) (map: char[,]) : (int * int) list =
    let rows, cols = Array2D.length1 map, Array2D.length2 map
    let visited = System.Collections.Generic.HashSet<(int * int)>()
    let isValid (x, y) = x >= 0 && x < rows && y >= 0 && y < cols
    let isPeak currentPos =
        let currentValue = charFromPos currentPos map
        let neighbors =
            dirs
            |> List.map (fun dir -> move dir 1 currentPos)
            |> List.filter isValid
        neighbors |> List.forall (fun neighbor ->
            let neighborValue = charFromPos neighbor map
            neighborValue <= currentValue
        )

    let rec dfs pos acc =
        if visited.Contains(pos) then acc
        else
            visited.Add(pos) |> ignore
            let currentValue = charFromPos pos map
            let neighbors =
                dirs
                |> List.map (fun dir -> move dir 1 pos)
                |> List.filter isValid
                |> List.filter (fun neighbor ->
                    let neighborValue = charFromPos neighbor map
                    neighborValue >= currentValue
                )
            let acc' = if isPeak pos then pos :: acc else acc
            neighbors |> List.fold (fun acc n -> dfs n acc) acc'

    dfs start []
    
findPeaks (0, 3) map