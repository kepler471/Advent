open System.Text.RegularExpressions

// Read in program as a single piece of text 
let program =
    System.IO.File.ReadAllText("2024/03/input")

// Separate patterns to match each operator
// This does mean we will need 3 passes through the text
let mulPattern = @"mul\((\d{1,3}),(\d{1,3})\)"
let doPattern = @"do\(\)"
let dontPattern = @"don't\(\)"

type Op =
| Mul of int * int * int // id, value a, value b
| Do of int // id
| Dont of int // id

module Op =
    let getId (op: Op) =
        match op with
        | Mul (x, _, _) -> x
        | Do x -> x
        | Dont x -> x

// Scan the text for each operator.
// Store the location that each op is found at
let mulMatches =
    Regex.Matches(program, mulPattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> Mul(m.Index, int m.Groups.[1].Value, int m.Groups.[2].Value))
    |> Seq.toList
let doMatches =
    Regex.Matches(program, doPattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> Do(m.Index))
    |> Seq.toList
let dontMatches =
    Regex.Matches(program, dontPattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> Dont(m.Index))
    |> Seq.toList

// Sort by location
let sortedOps = mulMatches @ doMatches @ dontMatches |> List.sortBy Op.getId

let parse (stack : Op list) =
    let rec step (stack : Op list) acc active =
        match stack with
        | [] -> acc
        | h :: t ->
            match h with
            | Mul (_, a, b) when active -> step t (acc + (a * b)) active
            | Mul _ -> step t acc active
            | Do _ -> step t acc true
            | Dont _ -> step t acc false
    step stack 0 true

printfn $"Part One: %d{parse mulMatches}"
printfn $"Part Two: %d{parse sortedOps}"