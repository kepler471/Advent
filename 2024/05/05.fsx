let raw = System.IO.File.ReadAllText("2024/05/input") |> _.Split("\n\n")
let rules = raw[0].Split("\n") |> Seq.map (_.Split("|") >> (fun x -> x[0], x[1]))
let updates = raw[1].Split("\n") |> Seq.map _.Split(",")

let filterRules (update: string seq) (rules: (string * string) seq) =
    Seq.filter (fun rule -> Seq.contains (fst rule) update && Seq.contains (snd rule) update) rules

let checkRule (update: string seq) (rule: string * string) : bool =
    let pageA, pageB =
        Seq.findIndex (fun x -> x = fst rule) update, Seq.findIndex (fun x -> x = snd rule) update

    pageA < pageB

// let (|@|) (a: string) (b: string) =
//     if checkRule [a; b] rules then -1  // a should come before b
//     elif checkRule [b; a] rules then 1  // b should come before a
//     else 0  // a and b are equal or unrelated

let ( |@| ) (rule: string * string) a b =
    let (x, y) = rule
    if a = x && b = y then -1 elif a = y && b = x then 1 else 0

 // "2" "1"
List.sortWith ((|@|) ("1", "2")) ["2"; "3"; "1"]

let compareByAllRules (rules: (string * string) seq) a b =
    rules
    |> Seq.fold (fun acc rule ->
        if acc = 0 then ( (|@|) rule a b) else acc) 0

let rec iter (rules: (string * string) list) update =
    match rules with
    | [] -> update
    | h :: t -> iter t (Array.sortWith ((|@|) h) update)
    
let test1 = updates |> Seq.toList |> List.item 5
iter (rules |> Seq.toList) test1 |> checkValid ....

let checkValidUpdate (rules: (string * string) seq) (update: string seq) : bool =
    rules |> Seq.map (checkRule update) |> Seq.forall id

let midval seq = (Seq.toList seq)[Seq.length seq / 2]

let filterValidUpdates updates =
    updates
    |> Seq.filter (fun update -> checkValidUpdate (filterRules update rules) update)

let sumMidpoints updates =
    updates |> Seq.map midval |> Seq.map int |> Seq.sum

let orderPages (rules: (string * string) seq) (update: string seq) : string seq =
    
    update

printfn $"Part One: %A{filterValidUpdates updates |> sumMidpoints}"
printfn $"Part Two: %A{filterValidUpdates updates |> sumMidpoints}"
