#load "../utils.fs"

open _2024.utils
open System.Collections.Generic

let map = System.IO.File.ReadAllLines("2024/12/test") |> array2D

let samePlot a b = charFromPos a map = charFromPos b map

let fillRegion (map: char array2d) (start: int * int) : (int * int) seq =
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
                    |> List.filter (fun pos' -> inbounds map pos' && samePlot pos pos')

                for neighbour in neighbours do
                    yield! step neighbour
        }

    step start

let chunker map =
    let uniquePlotChars = map |> Seq.cast<char> |> Seq.distinct
    let plotsByChar = uniquePlotChars |> Seq.map (fun lab -> lab, findChars lab map)

    seq {
        for char, plots in plotsByChar do
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


chunker map
|> Seq.toList
|> List.map (fun (c, p) -> c, Seq.length p, p |> List.map (countFencesAtPoint map) |> List.sum)
|> List.map (fun (_, area, perim) -> area * perim)
|> List.sum
