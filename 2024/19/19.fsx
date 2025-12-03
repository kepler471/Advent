#load "../utils.fs"

open System
open System.Collections.Concurrent
open Microsoft.FSharp.Core
open _2024.utils

let input = Input.paragraphs "2024/19/input"
let (towels: string array) = input[0].Split(", ")
let patterns = input[1].Split("\n")

let rec isPatternValid (pattern: string) =
    if String.IsNullOrEmpty(pattern) then
        true
    else
        towels
        |> Seq.filter pattern.StartsWith
        |> Seq.exists (fun (towel: string) -> isPatternValid(pattern.Substring(towel.Length)))

let patternArrangements (pattern: string) = 
    let patternArrangements' fn (pattern: string) =
        if String.IsNullOrEmpty(pattern) then
            1UL
        else
            towels
            |> Seq.filter pattern.StartsWith
            |> Seq.sumBy (fun (towel: string) -> fn(pattern.Substring(towel.Length)))
            
    let patternArrangementsMemo = memoiseRec patternArrangements'
    patternArrangementsMemo pattern

patterns |> Array.map isPatternValid |> Seq.filter id |> Seq.length
patterns |> Array.map patternArrangements |> Seq.sum
