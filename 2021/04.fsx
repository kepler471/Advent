let input = 
    "inputs/04.txt" |> (System.IO.File.ReadLines >> Seq.toList)

let draw = 
    (List.head input).Split [|','|] |> Array.map (fun x -> int x)

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

let getCol col (m: (int * bool) [,]) = 
    m.[*,col]

let getRow row (m: (int * bool) [,]) = 
    m.[row,*]

let isBingo marked = 
    [ for mark in marked -> snd mark ] |> List.reduce (&&)

// m |> getCol 3 |> isBingo;; 

// let solve draw boards = 
    // let doDraw draw boards bingo = 
    // 