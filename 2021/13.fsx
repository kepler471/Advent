(*
--- Day 13: Transparent Origami ---

You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.

Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:

Congratulations on your purchase! To activate this infrared thermal imaging
camera system, please enter the code found on page 1 of the manual.

Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:

6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5

The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........

Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
-----------
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........

Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:

#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........

Now, only 17 dots are visible.

Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.

Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.

The second fold instruction is fold along x=5, which indicates this line:

#.##.|#..#.
#...#|.....
.....|#...#
#...#|.....
.#.#.|#.###
.....|.....
.....|.....

Because this is a vertical line, fold left:

#####
#...#
#...#
#...#
#####
.....
.....

The instructions made a square!

The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.

How many dots are visible after completing just the first fold instruction on your transparent paper?

Your puzzle answer was 682.
--- Part Two ---

Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.

What code do you use to activate the infrared thermal imaging camera system?

Your puzzle answer was FAGURZHE.

Both parts of this puzzle are complete! They provide two gold stars: **
*)

#nowarn "57"

type Paper<'T> = 'T [,]
type direction = X | Y
type instruction = direction * int

let Fold (combine) (paper: 'T Paper) (instr: instruction) =    
    let n = (snd instr)
    match fst instr with
    | X -> 
        let top = paper.[*,0..n-1]
        top |> Array2D.mapi (fun i j x -> combine x paper.[i, paper.GetReverseIndex(1, j)])
    | Y -> 
        let top = paper.[0..n-1,*]
        top |> Array2D.mapi (fun i j x -> combine x paper.[paper.GetReverseIndex(0, i), j])

// Solve
let input = System.IO.File.ReadAllLines "2021/inputs/13.txt"

let dots = 
    input.[..819]
    |> Array.map (fun v ->
        let xy = v.Split [| ',' |]
        (int xy.[0],int xy.[1]))

let instructions = 
    input.[821..]
    |> Array.map (fun v -> 
        let i = v.[11..].Split [| '=' |]
        let dir = if i.[0] = "x" then X else Y
        instruction(dir, int i.[1]))

let dotsToPaper (init: 'T) (positions: (int * int) []): 'T Paper = 
    let (empty: 'T [,]) = 
        let m = positions |> Array.maxBy (fun (_, y) -> y) |> snd
        let n = positions |> Array.maxBy (fun (x, _) -> x) |> fst
        Array2D.zeroCreate (m+1) (n+1)
    positions |> Array.iter (fun p -> empty.[snd p, fst p] <- init)
    empty

let printCode replace printval (code: 'T [,]) = 
    [for i in 0..code.GetLength 0 - 1 ->
         printfn "%A" (code.[i,*] 
            |> Array.map (fun x -> if replace x then printval else " ") 
            |> String.concat "") 
    ]

printfn "--- Day 13: Transparent Origami ---"
printfn "Part One: %A" 
    (Fold (+) (dotsToPaper 1 dots) (Array.head instructions) |> Seq.cast<int> |> Seq.countBy id)
printfn "Part Two: %A" 
    (let d = dotsToPaper 1 dots
    instructions |> Seq.fold (fun acc x -> Fold (+) acc x) d |> printCode (fun x -> x >= 1) "#")