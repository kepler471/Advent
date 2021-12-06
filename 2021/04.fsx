let input = 
    "inputs/04.txt" |> (System.IO.File.ReadLines >> Seq.toList)

let draw = 
    (List.head input).Split [|','|] |> Array.map (fun x -> int x) 
    |> Array.toList

let boards = 
    input
    |> List.tail 
    |> List.toArray 
    |> Array.filter (fun x -> x <> "")
    |> Array.map (
        fun x -> 
        x.Split [|' '|] 
        |> Array.filter (fun x -> x <> "") 
        |> Array.map (fun x -> (int x, false))
    )
    |> Array.chunkBySize 5
    |> Array.map (fun x -> array2D x)
    // TODO: Use seq throughout
    // |> Array.toSeq

let getRow i (m: (int * bool) [,]) = 
    m.[i,*]

let getCol j (m: (int * bool) [,]) = 
    m.[*,j]

let isBingo line = 
    [ for mark in line -> snd mark ] |> List.reduce (&&)

// m |> getCol 3 |> isBingo;; 
let m = Array.head boards
// let xx, yy = m.GetUpperBound 1, m.GetLowerBound 0

let checkBingoBoard (i, j) board =
    board |> (getRow i >> isBingo) || board |> (getCol j >> isBingo)

let findMarkIndex n (board: (int * bool) [,]) =
    [ for i in 0..4 
        do yield! [for j in 0..4 
            do if board.[i,j] = (n, false) then Some(i, j) else None]
    ] |> List.filter (fun x -> Option.isSome x)

// [None; None; None; None] |> List.filter (fun v -> not (Option.isNone v))
// not None.IsNone

let sumUnmarked (m: (int * bool) [,]) = 
    let flat2Darray array2D = 
                seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                          for y in [0..(Array2D.length2 array2D) - 1] do 
                              yield array2D.[x, y] }
    m |> flat2Darray |> Seq.map (fun x -> if snd x = false then fst x else 0) |> Seq.sum

let drawNumbers draw boards = 
    let rec playBingo draw = 
        match draw with
        | [] -> 0
        | n::t -> 
            let stampPositions = 
                boards 
                |> Array.map (findMarkIndex n)
            // let winner = 
            //     (stampPositions, boards) 
            //     |> Array.map2 (fun x y -> checkBingoBoard x y)
            for (s, b) in stampPositions, boards do 
                let i, j = s
                b.[i,j] <- (n, true)
                if checkBingoBoard s b then n * sumUnmarked b
            playBingo t
    playBingo draw

        

checkBingoBoard (0, 1) m