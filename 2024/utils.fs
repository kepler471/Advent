module _2024.utils

module Input =
    let paragraphs file = System.IO.File.ReadAllText(file).Split("\n\n")
    let lines file = System.IO.File.ReadAllLines(file)
    let grid file = System.IO.File.ReadAllLines(file) |> array2D
    

type Point =
    { x: int; y: int }
    static member ofTuple (x: int * int) = { x = fst x; y = snd x }
    static member (+) (p1: Point, p2: Point) = { x = p1.x + p2.x; y = p1.y + p2.y }
    static member (-) (p1: Point, p2: Point) = { x = p1.x - p2.x; y = p1.y - p2.y }
    static member (*) (p: Point, scalar: int) = { x = p.x * scalar; y = p.y * scalar }
    static member (*) (scalar: int, p: Point) = { x = p.x * scalar; y = p.y * scalar }

let manhattan a b =
    abs(a.x - b.x) + abs(a.y - b.y)

let rec rep n (item: string) =
    match n with
    | 0 -> item
    | _ -> rep (n - 1) (item + item)

let iterN n fn =
    let rec iter fn' n' = if n' = 0 then fn' else iter (fn >> fn') (n' - 1)
    iter id n

let digitToInt (c: char) =
    if System.Char.IsAsciiDigit c then int c - int '0' else failwith "Input val is not digit char" 

let setCharAt (map: char array2d) (char: char) (at: Point) =
    map[at.x, at.y] <- char 

// TODO: Need some predicate function for search
//  It takes prev and curr positions, then says
//  which ones we can use. Its a get_neighbours() func. 

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

let moveOld dir dist pos =
    match dir with
    | Up -> fst pos - dist, snd pos
    | Do -> fst pos + dist, snd pos
    | Le -> fst pos, snd pos - dist
    | Ri -> fst pos, snd pos + dist
    
let move dir dist (pos: Point) =
    match dir with
    | Up -> pos + { x = - dist; y = 0 }
    | Do -> pos + { x = + dist; y = 0 }
    | Le -> pos + { x = 0; y = - dist }
    | Ri -> pos + { x = 0; y = + dist }
    
let move8 dir dist pos =
    match dir with
    | Nrth -> fst pos - dist, snd pos
    | Sout -> fst pos + dist, snd pos
    | West -> fst pos, snd pos - dist
    | East -> fst pos, snd pos + dist
    | NoWe -> fst pos - dist, snd pos - dist
    | NoEa -> fst pos + dist, snd pos + dist
    | SoWe -> fst pos, snd pos - dist - dist
    | SoEa -> fst pos, snd pos + dist + dist

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

let digitAt (pos: Point) (arr: char array2d) : int = digitToInt arr[pos.x, pos.y]
let charAt (pos: Point) (arr: char array2d) : char = arr[pos.x, pos.y]

let inbounds (map: char array2d) (pos: Point) =
    pos.x >= map.GetLowerBound 0 && pos.x <= map.GetUpperBound 0 &&
    pos.y >= map.GetLowerBound 0 && pos.y <= map.GetUpperBound 0

let printCharMap (map: char array2d) spacing =
    printf "  "
    for col in 0 .. Array2D.length2 map - 1 do
        let spacing' = rep spacing " "
        printf $"%u{col % 10}{spacing'}"
    printfn ""
    for row in 0 .. Array2D.length1 map - 1 do
        printf $"%u{row % 10} "
        for col in 0 .. Array2D.length2 map - 1 do
            let spacing' = rep spacing " "
            printf $"%c{map[row, col]}{spacing'}"
        printfn ""

let printIntMap (map: int array2d) spacing =
    for row in 0 .. Array2D.length1 map - 1 do
        for col in 0 .. Array2D.length2 map - 1 do
            let spacing' = rep spacing " "
            printf $"%i{map[row, col]}{spacing'}"
        printfn ""

let rec permutationsWithReplacement n list =
    match n with
    | 0 -> [ [] ]
    | _ ->
        list
        |> List.collect (fun x -> permutationsWithReplacement (n - 1) list |> List.map (fun perm -> x :: perm))

let aStar (grid: char array2d) (start: Point) (goal: Point) (heuristic: Point -> Point -> int) =
    let frontier = PriorityQueue<Point * Dirs, int>()
    frontier.Enqueue((start, Ri), 0)
    let from = Dictionary<Point, Point option>()
    let cost = Dictionary<Point, int>()
    from.Add(start, None)
    cost.Add(start, 0)
    let mutable Break = false
    
    while frontier.Count > 0 && not Break do
        let current, facing = frontier.Dequeue()
        
        if current = goal then
            printfn "REACHED THE BLOODY GOAL INNIT"
            cost[goal] <- cost[current] + 1
            from[goal] <- Some(current)
            Break <- true
        else
            let options = dirs |> List.filter (fun dir -> charAt (move dir 1 current) grid = '.')
            let neighbours = options |> List.map (fun dir -> move dir 1 current)
            
            for next, dir in List.zip neighbours options do
                let turnCost = if dir = facing then 0 else 1000
                let cost' = cost[current] + 1 + turnCost
                if cost.ContainsKey(next) |> not || cost' <= cost[next] then
                    cost[next] <- cost'
                    let priority = cost' + heuristic next goal
                    frontier.Enqueue((next, dir), priority)
                    from[next] <- Some(current)
    from, cost  

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
