#load "../utils.fs"

open _2025.utils 
open System.Collections.Generic

let banks = Input.lines "03/input" |> Seq.map (Seq.map digitToInt)

// Look from the right of the bank to the left
// Make the function flexible for the number of batteries
// Reverse the bank
// For n batteries, look for b_1 by skipping n - 1 batteries
// ... and so on
let maxJoltage (digits: int) (bank: int seq) =
    let rec findMaxNum (search: int seq) (stack: int list) (n: int) =
        if n = 0 then
            stack |> List.rev |> List.map int64 |> digitsToInt64 
        else
            let largestBattery = search |> Seq.rev |> Seq.skip (n - 1) |> Seq.max
            let batteryLocation = search |> Seq.findIndex ((=) largestBattery)
            findMaxNum (Seq.skip (batteryLocation + 1) search) (largestBattery :: stack) (n - 1)

    findMaxNum bank [] digits

banks |> Seq.map (maxJoltage 2) |> Seq.sum
banks |> Seq.map (maxJoltage 12) |> Seq.sum
