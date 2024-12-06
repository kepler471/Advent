let reports =
    System.IO.File.ReadLines("2024/02/input")
    |> Seq.map _.Split(" ")
    |> Seq.map (Array.map int)
    |> Seq.map Array.toList

// let isSafe (report: int list) =
//     let gradual a b = 3 >= abs (b - a) && 0 < abs (b - a)
//     let grad a b = sign (b - a)
//     
//     let rec findUnsafe rep direction =
//         match rep with 
//         | a :: b :: c :: t when gradual a b && (grad a b) = (grad b c) -> findUnsafe (b :: c :: t)
//         | a :: b :: [c] when gradual a b && (grad a b) = (grad a c) -> true
//         | _ -> false
//     
//     let increasing = grad report.[0] report.[1]
//     findUnsafe report increasing
//
// reports |> Seq.filter isSafe |> Seq.length


let isSafe2 (report: int list) =
    let gradual (a: int) (b: int) = 3 >= abs (b - a) && 0 < abs (b - a)
    let grad a b = sign (b - a)
    let diffs = report |> List.pairwise |> List.map (fun pair -> (snd pair) - (fst pair))
    
    let allGradual = diffs |> List.forall (fun x -> x <> 0 && abs x <= 3)
    let allSameDir = diffs |> List.forall (fun n -> sign n = sign diffs.[0])
    // let allSameDir = diffs |> List.reduce (fun acc n -> (sign acc) = (sign n))
    // let isGradual = diffs |> List.map gradual
    
    allGradual && allSameDir
    
// reports |> Seq.map isSafe2 |> List.ofSeq
// reports |> Seq.map isSafe2 |> List.ofSeq |> List.map (List.forall (fun x -> x <> 0 && abs x <= 3))
// reports |> Seq.map isSafe2 |> List.ofSeq |> List.map (List.forall (fun n -> sign n = sign diffs.[0]))

let countSafe safetyCheck reports= reports |> Seq.filter safetyCheck |> Seq.length


let rec checkSafeWithDampener safetyCheck (report: int list) =
    // match report with
    // List.sk
    let removeAt index lst = List.take index lst @ List.skip (index + 1) lst
    let variations =
              seq { for i in 0 .. (List.length report - 1) do yield removeAt i report }
    safetyCheck report || Seq.exists safetyCheck variations


printfn $"Part One: %d{countSafe isSafe2 reports}"
printfn $"Part Two: %d{countSafe (checkSafeWithDampener isSafe2) reports}"
