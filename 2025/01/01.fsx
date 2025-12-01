// Split the input into two columns
let (rotation, n) =
    System.IO.File.ReadLines("01/input")
    |> Seq.map (fun r -> int r.[0], int r.[1..])
    |> Seq.toList
    |> List.unzip