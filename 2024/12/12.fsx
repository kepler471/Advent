#load "../utils.fs"

open _2024.utils
open System.Collections.Generic

let map = System.IO. File.ReadAllLines("2024/12/input") |> array2D

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

let chunker (map: char array2d) (searchList: (char * (int * int) list) seq) =
    seq {
        for char, plots in searchList do
            let remaining = HashSet<int * int>()
            plots |> List.iter (fun x -> remaining.Add(x) |> ignore)

            while remaining.Count > 0 do
                let filled = fillRegion map (Seq.head remaining) |> Seq.toList
                yield char, filled
                filled |> Seq.iter (fun x -> remaining.Remove(x) |> ignore)
    }

let countFencesAtPoint (map: char array2d) (pos: int * int) =
    dirs
    |> List.filter (fun dir ->
        let pos' = move dir 1 pos
        not (inbounds map pos') || not (samePlot pos pos'))
    |> List.length

let areasAndPerim = 
    chunker map plotsByChar
    |> Seq.toList
    |> List.map (fun (c, p) -> c, Seq.length p, p |> List.map (countFencesAtPoint map) |> List.sum)
areasAndPerim
|> List.map (fun (c, area, perim) -> c, area * perim)

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
    chunker map plotsByChar
    |> Seq.toList
    |> List.map (fun (c, p) -> c, p |> List.collect (getPanelVecs map))
    |> List.map (fun (c, p) -> c, p |> List.groupBy fst)
    |> List.map (fun (c, p) -> c, p |> List.map snd)
    |> List.map (fun (c, p) -> c, p |> List.map (List.map snd))
    |> List.map (fun (c, p) -> c, p |> List.map (fun x -> c, x))


chunker' map (regionPanels |> List.collect (snd >> id)) |> Seq.toList

let sides = [
    for c, regionPanels' in regionPanels do
        // yield (c, c, regionPanels')
        yield c, chunker' map (regionPanels') |> Seq.toList |> Seq.length
]

(areasAndPerim, sides)
||> List.zip
|> List.map (fun ((_, a, _), (c, b)) -> a * b)
|> List.sum