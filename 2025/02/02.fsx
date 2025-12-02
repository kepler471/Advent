#load "../utils.fs"

open System.Text.RegularExpressions
open _2025.utils 

let parse_ids (ids: string array) =
    ids[0].Split(",") |> Seq.map (_.Split("-") >> (fun x -> int64 x[0], int64 x[1]))

let ids_raw = Input.lines "02/input" |> parse_ids
let ids_raw_test = Input.lines "02/test" |> parse_ids

let fillRange f (a, b) =
    seq { a..b } |> Seq.map f

let fillRangeAndStringify = fillRange string

let isSlightlySilly (input: string) =
    let len = input.Length
    input[..len/2-1] = input[len/2..]

// Source - https://stackoverflow.com/questions/55841427/how-to-check-if-string-has-repeating-pattern
// Posted by flakes, modified by community. See post 'Timeline' for change history
// Retrieved 2025-12-02, License - CC BY-SA 4.0
// ^(.+)(?:\1)+$
let isVerySilly (input: string) =
    Regex.IsMatch(input, @"^(.+)(?:\1)+$")

let slightlySillyFilter xs = Seq.filter isSlightlySilly xs
let verySillyFilter xs = Seq.filter isVerySilly xs

let part_1 = ids_raw |> Seq.sumBy (fillRangeAndStringify >> slightlySillyFilter >> Seq.sumBy int64)
let part_2 = ids_raw |> Seq.sumBy (fillRangeAndStringify >> verySillyFilter >> Seq.sumBy int64)