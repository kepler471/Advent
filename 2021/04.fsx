(*
--- Day 4: Giant Squid ---

You're already almost 1.5km (almost a mile) below the surface of the ocean, already so deep that you can't see any sunlight. What you can see, however, is a giant squid that has attached itself to the outside of your submarine.

Maybe it wants to play bingo?

Bingo is played on a set of boards each consisting of a 5x5 grid of numbers. Numbers are chosen at random, and the chosen number is marked on all boards on which it appears. (Numbers may not appear on all boards.) If all numbers in any row or any column of a board are marked, that board wins. (Diagonals don't count.)

The submarine has a bingo subsystem to help passengers (currently, you and the giant squid) pass the time. It automatically generates a random order in which to draw numbers and a random set of boards (your puzzle input). For example:

7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7

After the first five numbers are drawn (7, 4, 9, 5, and 11), there are no winners, but the boards are marked as follows (shown here adjacent to each other to save space):

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

After the next six numbers are drawn (17, 23, 2, 0, 14, and 21), there are still no winners:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

Finally, 24 is drawn:

22 13 17 11  0         3 15  0  2 22        14 21 17 24  4
 8  2 23  4 24         9 18 13 17  5        10 16 15  9 19
21  9 14 16  7        19  8  7 25 23        18  8 23 26 20
 6 10  3 18  5        20 11 10 24  4        22 11 13  6  5
 1 12 20 15 19        14 21 16 12  6         2  0 12  3  7

At this point, the third board wins because it has at least one complete row or column of marked numbers (in this case, the entire top row is marked: 14 21 17 24 4).

The score of the winning board can now be calculated. Start by finding the sum of all unmarked numbers on that board; in this case, the sum is 188. Then, multiply that sum by the number that was just called when the board won, 24, to get the final score, 188 * 24 = 4512.

To guarantee victory against the giant squid, figure out which board will win first. What will your final score be if you choose that board?

Your puzzle answer was 39902.
--- Part Two ---

On the other hand, it might be wise to try a different strategy: let the giant squid win.

You aren't sure how many bingo boards a giant squid could play at once, so rather than waste time counting its arms, the safe thing to do is to figure out which board will win last and choose that one. That way, no matter which boards it picks, it will win for sure.

In the above example, the second board is the last to win, which happens after 13 is eventually called and its middle column is completely marked. If you were to keep playing until this point, the second board would have a sum of unmarked numbers equal to 148 for a final score of 148 * 13 = 1924.

Figure out which board will win last. Once it wins, what would its final score be?

Your puzzle answer was 26936.

Both parts of this puzzle are complete! They provide two gold stars: **
*)

let getRow i (m: (int * bool) [,]) = 
    m.[i,*]

let getCol j (m: (int * bool) [,]) = 
    m.[*,j]

let isBingo line = 
    [ for mark in line -> snd mark ] |> List.reduce (&&)

let checkBingoBoard (i, j) board =
    ((board |> (getRow i >> isBingo)) && not( (board |> (getCol j >> isBingo))))
    || (not((board |> (getRow i >> isBingo))) && (board |> (getCol j >> isBingo)))

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
    m 
    |> flat2Darray 
    |> Seq.map (fun x -> if snd x = false then fst x else 0) 
    |> Seq.sum

let drawNumbers draw boards = 
    let mutable winners = []
    let rec playBingo draw  = 
        match draw with
        | [] -> winners |> List.rev |> List.distinctBy (fun (x, _) -> x)// all numbers drawn
        | n::t -> 
            let stampPositions = boards |> Seq.map (findMarkIndex n)
            for (s, b) in (Seq.zip stampPositions boards) do 
                let id = (boards |> Seq.findIndex (fun s -> b = s))
                match Seq.toArray s with
                | [|Some v|] -> 
                    let i, j = v
                    b.[i,j] <- (n, true)
                    if checkBingoBoard v b
                    then 
                        let score = n * sumUnmarked b
                        winners <- (id, score) :: winners    
                | _ -> ()
            playBingo t
    playBingo draw

// Solve
let input = 
    "inputs/04.txt" |> System.IO.File.ReadLines |> Seq.toList

let getDraw (input: string list)= 
    (List.head input).Split [|','|] |> Array.map (fun x -> int x) 
    |> Array.toList

let getBoards input = 
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

let BingoWinners = drawNumbers (getDraw input) (getBoards input)

printfn "--- Day 4: Giant Squid! ---"
printfn "Part One: (board id * score) -> %A" (BingoWinners |> List.rev |> List.head)
printfn "Part Two: (board id * score) -> %A" (BingoWinners |> List.head)
