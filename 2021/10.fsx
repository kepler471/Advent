let input = 
    System.IO.File.ReadAllLines "inputs/10.txt"

let inc x = (fst x + 1, snd x)
let dec x = (fst x, snd x - 1)
let sum x = fst x + snd x


let closed x = dec x |> sum <= 0
let moreopen x = fst x > snd x

let isClosing a b c d = 
       (closed a && (sum b > 0 || sum c > 0 || sum d > 0))

let score c = 
    match c with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> 0

let parse fn (line: string) = 
    let rec move paren brack brace chevr (line: char list) parsed =
        printfn "%A" (line |> System.String.Concat)
        match line, parsed with
        | [], _ -> fn '.' //(('(', paren), ('[', brack), ('{', brace), ('<', chevr))
        | '(' :: t, _ -> move (inc paren) brack brace chevr t ('(' :: parsed)
        | '[' :: t, _ -> move paren (inc brack) brace chevr t ('[' :: parsed)
        | '{' :: t, _ -> move paren brack (inc brace) chevr t ('{' :: parsed)
        | '<' :: t, _ -> move paren brack brace (inc chevr) t ('<' :: parsed)
        | ')' :: t when List.head parsed = '(' -> 
            move (dec paren) brack brace chevr t (List.tail parsed)
        | ')' :: _ -> fn ')'
        | ']' :: t when List.head parsed = '[' -> 
            move paren (dec brack) brace chevr t (List.tail parsed)
        | ']' :: _ -> fn ']'
        | '}' :: t when List.head parsed = '{' -> 
            move paren brack (dec brace) chevr t (List.tail parsed)
        | '}' :: _ -> fn '}'
        | '>' :: t when List.head parsed = '<' ->  
            move paren brack brace (dec chevr) t (List.tail parsed)
        | '>' :: _ -> fn '>'
    move (0, 0) (0, 0) (0, 0) (0, 0) (line |> Seq.toList) []

"{([(<{}[<>[]}>{[]{[(<()>" |> parse id // ] .. }
"[[<[([]))<([[{}[[()]]]" |> parse id   // ] .. )
"[{[{({}]{}}([{[{{{}}([]" |> parse id  // ) .. ]
"[{[{({}]"
"[<(<(<(<{})><([]([]()" |> parse id   // > .. )
"<{([([[(<>()){}]>(<<{{" |> parse id   // ] .. >
"<{([([          >(<<{{"

input |> Array.map (parse score) |> Array.sum
