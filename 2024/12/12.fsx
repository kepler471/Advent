#load "../utils.fs"

open _2024.utils
open System.Collections.Generic

let map = System.IO. File.ReadAllLines("2024/12/test") |> array2D

let samePlot a b = charAt a map = charAt b map

let fillRegion (map: char array2d) (start: int * int) : (int * int) seq =
    let visited = HashSet<int * int>()

    let rec step (pos: int * int) =
        seq {
            if visited.Contains(pos) then
                ()
            else
                visited.Add(pos) |> ignore
                yield pos

                let neighbours =
                    dirs
                    |> List.map (fun dir -> move dir 1 pos)
                    |> List.filter (fun pos' -> inbounds map pos' && samePlot pos pos')

                for neighbour in neighbours do
                    yield! step neighbour
        }

    step start

let uniquePlotChars = map |> Seq.cast<char> |> Seq.distinct
let plotsByChar: (char * (int * int) list) seq = uniquePlotChars |> Seq.map (fun lab -> lab, findChars lab map)

let chunker (map: char array2d) (searchList: (char * (int * int) list) seq) filler =
    seq {
        for char, plots in searchList do
            let remaining = HashSet<int * int>()
            plots |> List.iter (fun x -> remaining.Add(x) |> ignore)

            while remaining.Count > 0 do
                let filled = filler map (Seq.head remaining) |> Seq.toList
                yield char, filled
                filled |> Seq.iter (fun x -> remaining.Remove(x) |> ignore)
    }
let chunker' (map: char array2d) (searchList: (char * (int * int) list) seq) =
    seq {
        for char, plots in searchList do
            let remaining = HashSet<int * int>()
            plots |> List.iter (fun x -> remaining.Add(x) |> ignore)

            while remaining.Count > 0 do
                let filled = fillRegion' map (Seq.head remaining) plots |> Seq.toList
                yield char, filled
                filled |> Seq.iter (fun x -> remaining.Remove(x) |> ignore)
    }
let countFencesAtPoint (map: char array2d) (pos: int * int) =
    dirs
    |> List.filter (fun dir ->
        let pos' = move dir 1 pos
        not (inbounds map pos') || not (samePlot pos pos'))
    |> List.length


chunker map plotsByChar
|> Seq.toList
|> List.map (fun (c, p) -> c, Seq.length p, p |> List.map (countFencesAtPoint map) |> List.sum)
|> List.map (fun (_, area, perim) -> area * perim)
|> List.sum

let getPanelVecs (map: char array2d) (pos: int * int) =
    dirs
    |> List.filter (fun dir ->
        let pos' = move dir 1 pos
        not (inbounds map pos') || not (samePlot pos pos'))
    |> List.map (fun dir -> dir, pos)

let fillRegion' (map: char array2d) (start: int * int) (subregion: (int * int) list) : (int * int) seq =
    let visited = HashSet<(int * int)>()

    let rec step (pos: int * int) =
        seq {
            if visited.Contains(pos) then
                ()
            else
                visited.Add(pos) |> ignore
                yield pos

                let neighbours =
                    dirs
                    |> List.map (fun dir -> move dir 1 pos)
                    |> List.filter (fun pos' -> inbounds map pos' && samePlot pos pos' && List.contains pos' subregion)

                for neighbour in neighbours do
                    yield! step neighbour
        }

    step start

let chunker' (map: char array2d) (searchList: (char * (int * int) list) seq) =
    seq {
        for char, plots in searchList do
            let remaining = HashSet<int * int>()
            plots |> List.iter (fun x -> remaining.Add(x) |> ignore)

            while remaining.Count > 0 do
                let filled = fillRegion' map (Seq.head remaining) plots |> Seq.toList
                yield char, filled
                filled |> Seq.iter (fun x -> remaining.Remove(x) |> ignore)
    }

let regionPanels =
    chunker map plotsByChar fillRegion
    |> Seq.toList
    |> List.map (fun (c, p) -> c, p |> List.collect (getPanelVecs map))
    |> List.map (fun (c, p) -> c, p |> List.groupBy fst)
    |> List.map (fun (c, p) -> c, p |> List.map snd)
    |> List.map (fun (c, p) -> c, p |> List.map (List.map snd))
    |> List.map (fun (c, p) -> c, p |> List.map (fun x -> c, x))


chunker map (regionPanels |> List.collect (snd >> id)) fillRegion |> Seq.toList

[
    for c, regionPanels' in regionPanels do
        // yield (c, c, regionPanels')
        yield c, chunker' map (regionPanels') |> Seq.toList |> Seq.length
]


let test1 =  [('R', [(0, 0); (0, 1); (0, 2); (0, 3); (2, 4)]);
   ('R', [(0, 0); (2, 2); (1, 0); (3, 2)])]
chunker map test1


chunker map plotsByChar
|> Seq.toList |> List.head |> snd
|> List.collect (getPanelVecs map)
|> List.groupBy fst
|> List.map snd
|> List.map (List.map snd)

let side = HashSet<int*int>()
let plots = HashSet<int*int>()
[(0, 4); (0, 5); (0, 8)] |> List.iter (fun x -> plots.Add(x) |> ignore)
seq {
    while plots.Count > 0 do
        let head = plots |> Seq.head
        
}