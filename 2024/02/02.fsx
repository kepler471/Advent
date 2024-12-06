let reports =
    System.IO.File.ReadLines("2024/02/input")
    |> Seq.map _.Split(" ")
    |> Seq.map (Array.map int)
    |> Seq.map Array.toList

let checkSafe (report: int list) =
    let diffs = report |> List.pairwise |> List.map (fun pair -> (snd pair) - (fst pair))
    let allGradual = diffs |> List.forall (fun x -> x <> 0 && abs x <= 3)
    let allSameDir = diffs |> List.forall (fun n -> sign n = sign diffs.[0])
    allGradual && allSameDir
let countSafe safetyCheck reports= reports |> Seq.filter safetyCheck |> Seq.length

let rec checkSafeWithDampener safetyCheck (report: int list) =
    let removeAt index lst = List.take index lst @ List.skip (index + 1) lst
    let variations = seq { for i in 0 .. (List.length report - 1) do yield removeAt i report }
    safetyCheck report || Seq.exists safetyCheck variations

printfn $"Part One: %d{countSafe checkSafe reports}"
printfn $"Part Two: %d{countSafe (checkSafeWithDampener checkSafe) reports}"