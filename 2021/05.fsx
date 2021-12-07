let input = 
    "inputs/05.txt" 
    |> System.IO.File.ReadLines 
    |> Seq.map (fun x -> x.Replace(" -> ", ","))
    |> Seq.map (fun x -> x.Split [|','|])
    |> Seq.map (fun x -> [|for i in x -> int i|])

let removeDiagonal (lines: int [] seq) =
    lines
    |> Seq.filter (fun x -> x.[0] = x.[2] || x.[1] = x.[3])

let generateGrid height width = 
    Array2D.create height width 0

let (.+) a b = 
    Array2D.map (fun x -> x + b) a

let buildOnGrid (l: int []) (g: int [,]) = 
    g.[l.[0]..l.[2],l.[1]..l.[3]] <- g.[l.[0]..l.[2],l.[1]..l.[3]] .+ 1
    g

let parseLine (line: int []) = 
    if line.[0] > line.[2]
    then (line.[0], line.[1]), (line.[2], line.[3])
    elif line.[1] > line.[3]
    then (line.[0], line.[1]), (line.[2], line.[3])
    else (line.[0], line.[1]), (line.[2], line.[3])

input |> Seq.map parseLine

let x = fst
let y = snd
// Instead of marking matrix, count repeated coordinates of all lines?
let buildLine (a, b) =
    let (ax, ay), (bx, by) = (a, b)
    let inc = 
        match ax, ay, bx, by with
        | _ when ax > bx && ay > by -> (-1, -1)
        | _ when ax > bx && ay < by -> (-1,  1)
        | _ when ax < bx && ay > by -> (1,  -1)
        | _ -> (1,  1)

    [| for i in x(a)..y(b) do
            for j in snd a..snd b do yield (i,j) |]




let buildLine2 (l: int []) =
    [| for i in l.[0]..l.[2] do
            for j in l.[0]..l.[2] do yield (i,j) |]

let removeDiagonalLines lines = 
    lines |> Seq.filter (fun (a, b) -> fst a = fst b || snd a = snd b)

let newans = 
    input 
    |> Seq.map parseLine
    |> removeDiagonalLines 
    // |> Seq.map (fun (a, b) -> buildLine (a, b))

newans
seq { for i in newans do yield buildLine i}
((657,934),(657,926)) |> buildLine
//  [((657, 934), (657, 926));
input |> removeDiagonal
input |> Seq.map parseLine |> Seq.head
input |> Seq.map parseLine |> Seq.filter (fun (a, b) -> fst a = fst b || snd a = snd b)
input |> Seq.map parseLine |> Seq.filter (fun (a, b) -> fst a = 657)
let x,y = (input |> Seq.map parseLine |> Seq.head)
fst x = fst y
buildLine (1,1) (3,1)

let x lines = 
    let g = generateGrid 1000 1000
    let rec go lines grid = 
        match lines with
        | [] -> grid
        | h :: t -> 
            go t (buildOnGrid h grid)
    go (Seq.toList lines) g

let ans = input |> removeDiagonal |> x

let flat2Darray array2D = 
                seq { for x in [0..(Array2D.length1 array2D) - 1] do 
                          for y in [0..(Array2D.length2 array2D) - 1] do 
                              yield array2D.[x, y] }

flat2Darray ans |> Seq.map (fun x -> if x >= 2 then 1 else 0) |>  Seq.sum
flat2Darray newans

seq { for i in 0..999 do 
        for j in 0..999 do 
            yield if ans.[100,j] >= 2 then 1 else 0
} |> Seq.sum

let ans2 = 
    let mutable count = 0
    for i in 0..0999 do
        for j in 0..999 do
            if ans.[i,j] >= 2 then count <- count + 1
    count


let g = generateGrid 10 10
g.[0..2, 0..2] |> Array2D.map (fun x -> x + 2)
g.[0..2, 0..2] <- g.[0..2, 0..2] .+ 2
g
g
let testin = (input |> Seq.head)
let testin2 = [|1;1;1;8|]
buildOnGrid testin g
g.[5..5,0..9] |> Array2D.mapi (fun i j x -> Array2D.set g (i+4) j 69) 
g.[5..5,0..9] <- g.[5..5,0..9] .+ 101
g.[1,8] + 222
g

let testinput = [|0;0;0;999|]::Seq.toList input |> List.toSeq
testinput
let ans = x testinput
let ans = x input |> Array2D.iter (fun x -> if x <> 0 then printfn "%A" x)

g.[1..9,9..9] <- g.[1..9,9..9] .+ 23
g
Array2D.set
// "123,456 -> 234,345".Replace(" -> ", ",")
// // let isHorizontal (a, b) = 
// g.[0..4,2] <- Array.create 5 1
// Array2D.create 1 5 1
// Array.create 5 1
// "1234".[1]
// g
// // g.[0..4,2] <- g.[0..4,2] .+ 1
// g

//     //Array.create (l.[2]-l.[0]) (l.[3]-l.[1]) 1
// // let (.+) (a: int [], b: int) = 
// //     a |> Array.map (fun x -> x + b)



// [|1;2;3;4|] .+ 2
// g.[0..2, 0..2] <- g.[0..2, 0..2] .+ Array2D.create 3 3 1
// // 4l.GetType()

// g.[0..2, 2..4] |> Array2D.map (fun x -> x + 1)
// let (.+) (a: int) (b: int []) = 
//     Array.map (fun x -> x + a) b

// Array.zeroCreate 10

// type Array = Array with
//     static member inline (.+) ((a: int []), (b: int)) = fun Array.map (fun x -> x + b) a
//     static member inline (.+) ((a: int), (b: int [])) = Array.map (fun x -> x + a) b


// type Array = Array with
//     static member inline (.+) (a: int [], b: int) = fun (b: int   ) -> a |> Array.map (fun x -> x + b)
//     static member inline (.+) (a: int, b: int []) = fun (b: int []) -> b |> Array.map (fun x -> x + a)



// let inline (.+) (a: int, b: int []) = 
//     b |> Array.map (fun x -> x + a)

// (.+)