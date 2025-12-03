#load "../utils.fs"

open _2024.utils

let map = System.IO.File.ReadAllLines("2024/10/test") |> array2D

let trailheads = findChars '0' map

let isStepSize n a b = abs (a.x - by) + abs (a.x - b.y) <= n

let isGradualStep a b = (digitAt b map) - (digitAt a map) = 1 

// let traverse (map: char array2d) (start: int * int)=
//     let visited = System.Collections.Generic.HashSet<(int * int)>()
//
//     let rec loop pos = seq {
//         // if visited.Contains(pos) then
//         //     ()
//         // else
//             visited.Add(pos) |> ignore
//             let height = digitFromPos pos map
//             yield (pos, height)
//
//             let neighbours =
//                 dirs
//                 |> List.map (fun dir -> move dir 1 pos)
//                 |> List.filter (fun pos' -> inbounds map pos' && isGradualStep pos pos')
//
//             for neighbor in neighbours do
//                 yield! loop neighbor
//     }
//
//     loop start
    
let traverse (map: char array2d) (start: Point) =
    let visited = System.Collections.Generic.HashSet<Point>()

    let rec loop pos path = seq {
        let height = digitAt pos map
        let path' = (pos, height) :: path
        visited.Add(pos) |> ignore
        
        if height = 9 then yield path'
        
        let neighbours =
            dirs
            |> List.map (fun dir -> move dir 1 pos)
            |> List.filter (fun pos' -> inbounds map pos' && isGradualStep pos pos')

        for neighbor in neighbours do
            yield! loop neighbor path'
    }

    loop start []    

let calculateTrailheadScore (map: char array2d) (start: int * int) =
    traverse map start
    |> Seq.filter (fun (_, value) -> value = 9)
    |> Seq.length    

trailheads |> List.map (traverse map >> Seq.toList)
trailheads |> List.map (calculateTrailheadScore map) |> List.sum
