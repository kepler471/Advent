let getRow i (m: (int * bool) [,]) = 
    m.[i,*]

let getCol j (m: (int * bool) [,]) = 
    m.[*,j]

let isBingo line = 
    [ for mark in line -> snd mark ] |> List.reduce (&&)

let checkBingoBoard (i, j) board =
    board |> (getRow i >> isBingo) || board |> (getCol j >> isBingo)

let findMarkIndex n (board: (int * bool) [,]) =
    seq { for i in 0..4 
        do yield! [for j in 0..4 
            do if board.[i,j] = (n, false) then Some(i, j) else None]
     } |> Seq.filter (fun x -> Option.isSome x)

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
                |> Seq.map (findMarkIndex n)
            for (s, b) in (Seq.zip stampPositions boards) do 
                match Seq.toArray s with
                | [|Some v|] -> 
                    let i, j = v
                    b.[i,j] <- (n, true)
                    if checkBingoBoard v b
                    then printfn "Draw: %A, Winner:, Score: %A" n (n * sumUnmarked b)
                | _ -> ()
            playBingo t
    playBingo draw

let input = 
    "inputs/04.txt" |> System.IO.File.ReadLines |> Seq.toList

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
    |> Array.toSeq
    // TODO: Use seq throughout

drawNumbers draw boards