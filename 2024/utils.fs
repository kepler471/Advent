module _2024.utils

// TODO: use open System.Drawing to use the Point type


let digitToInt (c: char) =
    if System.Char.IsAsciiDigit c then int c - int '0' else failwith "Input val is not digit char" 

type Dirs =
    | Up
    | Do
    | Le
    | Ri

type DirsAlt =
    | UpLe
    | UpRi
    | DoLe
    | DoRi

type Dirs8 =
    | Nrth
    | Sout
    | West
    | East
    | NoWe
    | NoEa
    | SoWe
    | SoEa

let dirs = [ Up; Ri; Do; Le ]
let dirsAlt = [ UpLe; UpRi; DoRi; DoLe ]
let dirs8 = [ Nrth; Sout; West; East; NoWe; NoEa; SoWe; SoEa ]

type rotation =
    | Clockwise
    | AntiClockwise

let rotate items (rot: rotation) item =
    let idx = List.findIndex ((=) item) items
    let len = List.length items

    match rot with
    | Clockwise -> List.item ((idx + 1) % len) items
    | AntiClockwise -> List.item ((idx - 1 + len) % len) items

let move dir dist pos =
    match dir with
    | Up -> fst pos - dist, snd pos
    | Do -> fst pos + dist, snd pos
    | Le -> fst pos, snd pos - dist
    | Ri -> fst pos, snd pos + dist

let testEach dirs testFn = List.iter testFn dirs

let findChars (char: char) (arr: char array2d) =
    arr
    |> Array2D.mapi (fun i j c -> if c = char then Some(i, j) else None)
    |> Seq.cast<Option<int * int>>
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.toList

let rec comb n l =
    match n, l with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, (x :: xs) -> List.map ((@) [ x ]) (comb (k - 1) xs) @ comb k xs


let rec permutationsWithReplacement n list =
    match n with
    | 0 -> [ [] ]
    | _ ->
        list
        |> List.collect (fun x -> permutationsWithReplacement (n - 1) list |> List.map (fun perm -> x :: perm))

type Matrix<'T>(N: int, M: int) =
    let internalArray = Array2D.zeroCreate<'T> N M

    member this.Item
        with get (a: int, b: int) = internalArray[a, b]
        and set (a: int, b: int) (value: 'T) = internalArray[a, b] <- value

    member this.GetSlice(rowStart: int option, rowFinish: int option, colStart: int option, colFinish: int option) =
        let rowStart =
            match rowStart with
            | Some(v) -> v
            | None -> 0

        let rowFinish =
            match rowFinish with
            | Some(v) -> v
            | None -> internalArray.GetLength(0) - 1

        let colStart =
            match colStart with
            | Some(v) -> v
            | None -> 0

        let colFinish =
            match colFinish with
            | Some(v) -> v
            | None -> internalArray.GetLength(1) - 1

        internalArray[rowStart..rowFinish, colStart..colFinish]

    member this.GetSlice(row: int, colStart: int option, colFinish: int option) =
        let colStart =
            match colStart with
            | Some(v) -> v
            | None -> 0

        let colFinish =
            match colFinish with
            | Some(v) -> v
            | None -> internalArray.GetLength(1) - 1

        internalArray[row, colStart..colFinish]

    member this.GetSlice(rowStart: int option, rowFinish: int option, col: int) =
        let rowStart =
            match rowStart with
            | Some(v) -> v
            | None -> 0

        let rowFinish =
            match rowFinish with
            | Some(v) -> v
            | None -> internalArray.GetLength(0) - 1

        internalArray[rowStart..rowFinish, col]

module test =
    let generateTestMatrix x y =
        let matrix = new Matrix<float>(3, 3)

        for i in 0..2 do
            for j in 0..2 do
                matrix[i, j] <- float (i) * x - float (j) * y

        matrix

    let test1 = generateTestMatrix 2.3 1.1
    let submatrix = test1[0..1, 0..1]
    printfn $"{submatrix}"

    let firstRow = test1[0, *]
    let secondRow = test1[1, *]
    let firstCol = test1[*, 0]
    printfn $"{firstCol}"

let flattenArray2D (arr: 'T[,]) =
    seq {
        for i in 0 .. Array2D.length1 arr - 1 do
            for j in 0 .. Array2D.length2 arr - 1 do
                yield arr.[i, j]
    }

let flattenWithIndices (arr: 'T[,]) =
    seq {
        for i in 0 .. Array2D.length1 arr - 1 do
            for j in 0 .. Array2D.length2 arr - 1 do
                yield (i, j, arr.[i, j])
    }

let findIndicesOf (char: char) (arr: char[,]) =
    flattenWithIndices arr
    |> Seq.filter (fun (_, _, value) -> value = char) 
    |> Seq.map (fun (i, j, _) -> (i, j))
    |> Seq.toList

let findXs = findIndicesOf 'X'
let findMs = findIndicesOf 'M'
