// Split the input into two columns
let (left, right) =
    System.IO.File.ReadLines("2024/01/input")
    |> Seq.map _.Split("   ")
    |> Seq.map (fun pair -> int pair.[0], int pair.[1])
    |> Seq.toList
    |> List.unzip

let pairedDifferences x y =
    (List.sort y, List.sort x) ||> List.map2 (fun yi xi -> abs (yi - xi)) |> List.sum

let similarityScore x y = 
    let countsTable = y |> List.countBy id |> Map.ofList
    x
    |> List.fold (fun acc key ->
        match Map.tryFind key countsTable with
        | Some value -> acc + (key * value)
        | None -> acc
        ) 0

printfn $"Part One: %d{pairedDifferences left right}"
printfn $"Part Two: %d{similarityScore left right}"