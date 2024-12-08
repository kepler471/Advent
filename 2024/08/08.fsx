#load "../utils.fs"

open _2024.utils

type AntennaPair =
    { Coord1: (int * int)
      Coord2: (int * int)
      Antinodes: (int * int) list }

let map = System.IO.File.ReadLines("2024/08/input") |> array2D

let uniqueFrequencies map =
    map
    |> Seq.cast<char>
    |> Seq.distinct
    |> Seq.filter System.Char.IsAsciiLetterOrDigit

let groupAntennas map =
    uniqueFrequencies map |> Seq.map (fun c -> (c, findChars c map)) |> Map.ofSeq

let calculateAntinodes (x1, y1) (x2, y2) n =
    let dx, dy = x2 - x1, y2 - y1

    [ for i in 1..n do
          yield (x2 + i * dx, y2 + i * dy)
          yield (x1 - i * dx, y1 - i * dy) ]

let findAntinodesForGroup (coordinates: (int * int) list) n =
    comb 2 coordinates
    |> List.map (function
        | [ coord1; coord2 ] ->
            { Coord1 = coord1
              Coord2 = coord2
              Antinodes = calculateAntinodes coord1 coord2 n }
        | _ -> failwith "Should never occur. Can I make comb better so this case is not required?")

let processGrid map n =
    let antennaGroups = groupAntennas map
    let results =
        antennaGroups
        |> Map.map (fun frequency coordinates ->
            findAntinodesForGroup coordinates n)
    
    results
    |> Map.iter (fun freq pairs ->
        printfn $"Frequency %c{freq}:"
        pairs |> List.iter (fun pair ->
            printfn $"  Pair: (%A{pair.Coord1}, %A{pair.Coord2})"
            printfn $"  Antinodes: %A{pair.Antinodes}"))

    results // Return the results as a map

let extractAllAntinodes (result: Map<char, AntennaPair list>) =
    result
    |> Map.toSeq // Convert Map to sequence of (char, AntennaPair list)
    |> Seq.collect (fun (_, pairs) ->
        pairs |> Seq.collect (fun pair -> pair.Antinodes)) // Flatten antinodes from each pair
    |> Seq.toList // Convert to a single list

let filterAntinodesWithinGrid (antinodes: (int * int) list) (rows: int, cols: int) =
    antinodes
    |> List.filter (fun (x, y) -> x >= 0 && x < rows && y >= 0 && y < cols)

let result = processGrid map 1
let allAntinodes = extractAllAntinodes result
let gridSize = (Array2D.length1 map, Array2D.length2 map) // (rows, cols)
let validAntinodes = filterAntinodesWithinGrid allAntinodes gridSize

validAntinodes |> List.distinct |> List.length
validAntinodes |> List.append (groupAntennas map |> Map.values |> Seq.toList |> List.concat) |> List.distinct |> List.length
