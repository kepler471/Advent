#load "../utils.fs"

open _2025.utils 

let manifold = Input.lines "07/test" |> List.ofArray

// Options

// Go row by row through the manifold
// check for splitters
// simply propagate

let drawBeam c' ids str =
    let idSet = set ids
    str |> String.mapi (fun j c ->
        if idSet.Contains j then c' else c)
    
let splitDecider (a: string) (b: string) =
    [ for i, x in Seq.indexed a do
      if b[i] = '^' then [ i - 1; i + 1 ]
      elif x = '|' then [ i ] ]
    |> List.collect id

let tachyonBeamerOld (manifold: string list) =
    
    let rec propagateTachyons n acc (manifold': string list) =
        // if manifold.Head.Contains("S") then
        match manifold' with
        | cur :: nxt :: t when cur.Contains("S") ->
            let ids = findChars 'S' cur
            let nxt' = drawBeam '|' ids nxt 
            propagateTachyons n [ cur ] (nxt' :: t)
        | cur :: nxt :: t ->
            let tachyons' = splitDecider cur nxt
            let nxt' = drawBeam '|' (set tachyons') nxt
            let counter = if cur.Contains("^") then List.length tachyons' else 0
            propagateTachyons (n + counter) (cur :: acc) (nxt' :: t)
        | [] | _ -> n, List.rev acc 

    propagateTachyons 0 [] manifold

let tachyonBeamer (manifold: string list) =
    
    let rec propagateTachyons n acc (manifold': string list) =
        // if manifold.Head.Contains("S") then
        match manifold' with
        | cur :: nxt :: t when cur.Contains("S") ->
            let ids = findChars 'S' cur
            let nxt' = drawBeam (int '0' + 1 |> char) ids nxt 
            propagateTachyons n [ cur ] (nxt' :: t)
        | cur :: nxt :: t ->
            let tachyons' = splitDecider cur nxt
            let tachyonMap = tachyons' |> List.countBy id
            let mutable nxt' = nxt
            for i, n in tachyonMap do
                nxt' <- drawBeam (int '0' + n |> char) [i] nxt'
            let counter = if nxt.Contains("^") then List.length tachyons' else 0
            propagateTachyons (n + counter) (cur :: acc) (nxt' :: t)
        | [] | _ -> n, List.rev acc 

    propagateTachyons 0 [] manifold

let countTachyonCollisions manifold =
    findIndicesOf '^' manifold
    |> List.countWhere (fun (i, j) ->
        let above = i - 1, j
        manifold[fst above, snd above] = '|')

let countWorlds (manifold: string list) =
    manifold
    |> List.filter _.Contains("^")
    |> List.map (findChars '|')
    |> List.collect id
    |> List.length
    

// findIndicesOf '|' (tachyonBeamer manifold |> array2D) |> Seq.length
tachyonBeamer manifold |> snd |> array2D |> printCharMap 0
tachyonBeamer manifold |> snd |> array2D |> countTachyonCollisions
tachyonBeamer manifold |> snd |> countWorlds
tachyonBeamer manifold |> fst

//
// let a = ".|..|...||.."
// let b = "....^...^..."
// let c = splitDecider a b
// let d = drawBeam '|' c b

