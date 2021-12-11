let input = 
    System.IO.File.ReadAllLines "inputs/10.txt"

let score c = 
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let isOpeningBracket b = b = '(' || b = '[' || b = '{' || b = '<'
let isClosingBracket b = b = ')' || b = ']' || b = '}' || b = '>'
let bracketMatches b = 
    match b with
    | '(' -> (fun x -> x = ')')
    | ')' -> (fun x -> x = '(')
    | '[' -> (fun x -> x = ']')
    | ']' -> (fun x -> x = '[')
    | '{' -> (fun x -> x = '}')
    | '}' -> (fun x -> x = '{')
    | '<' -> (fun x -> x = '>')
    | '>' -> (fun x -> x = '<')

let matchBracket b = 
    match b with
    | '(' -> ')'
    | ')' -> '('
    | '[' -> ']'
    | ']' -> '['
    | '{' -> '}'
    | '}' -> '{'
    | '<' -> '>'
    | '>' -> '<'

let parse fn (line: string) = 
    printfn "%A" line
    let rec move (line: char list) stack =
        match line, stack with
        | [], _ -> 
            printfn "%A" stack
            fn ' ' //(('(', paren), ('[', brack), ('{', brace), ('<', chevr))
        | h :: t, _ when isOpeningBracket h-> move t (h :: stack)
        | h :: t, top :: bot when isClosingBracket h && h = matchBracket top -> move t bot
        | h :: _, _ -> fn h
    move (line |> Seq.toList) []

input |> Array.map (parse id)
input |> Array.map (parse score) |> Array.sum

input |> Array.filter (parse (fun x -> if x = ' ' then true else false))