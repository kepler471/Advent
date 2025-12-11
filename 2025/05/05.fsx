#load "../utils.fs"

open _2025.utils 
open System.Collections.Generic

let raw = Input.lines "05/input"

let freshIds =
    raw |> Array.takeWhile ((<>) "") |> Array.map (_.Split("-") >> Array.map int64) 

let ingredients =
    raw |> Array.rev |> Array.takeWhile ((<>) "") |> Array.rev |> Array.map int64

// For each available ingredient
// Check if it is >= one of the range starts
// -> then check if it is smaller than that same range close.
let checkInRange (id: int64) (range: int64 array) : bool =
    range[0] <= id && id <= range[1]

let checkFreshness (ranges: int64 array array) (id: int64) =
    ranges
    |> Seq.map (checkInRange id) |> Seq.contains true

ingredients |> Array.countBy (checkFreshness freshIds)

// To count the total number of fresh IDs.....
// Sort all ranges by the range open
// take the 2nd range, check if range open is within the previous range
// if FALSE, it must be greater than previous range close, no overlap
// THEN stash previous range, and move onto check 3rd range against 2nd etc...
// if TRUE, it must have some overlap with 1st range
// THEN we must merge these ranges into one. Merge by keeping the min of the range
// opens, and the max of the range closes.
// Do not stash this range, add it back to the list to check it against the next range
let countFreshIds (ranges: int64 array array) =
    let sortedRanges = ranges |> Array.sortBy (fun x -> x[0]) |> Array.toList
    
    let rec findRanges (stash: int64 array list) (remaining: int64 array list) =
        match remaining with
        | one :: two :: t when two[0] > one[1] ->
            findRanges (one :: stash) (two :: t)
        | one :: two :: t ->
            let newRange = [| min one[0] two[0]; max one[1] two[1] |]
            findRanges stash (newRange :: t)
        | [ t ] ->
            t :: stash |> List.rev
        | [] -> failwith "todo"

    let combinedRanges = findRanges [] sortedRanges
    {|
       ranges = combinedRanges
       total = combinedRanges |> List.sumBy (fun x -> x[1] - x[0] + 1L)
    |}

countFreshIds freshIds

// TODO: For visualisation - use the ranges indices, assign a y for each.
