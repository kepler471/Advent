module _2025.utils

open System
open FSharp
// open FSharpAux
// open FSharpAux
// open Plotly.NET
open System.Collections.Generic
open System.Collections.Concurrent
open System.Text.RegularExpressions

// TODO: Add fn to iterate through a grid/matrix by rows

let memoise f =
    let cache = ConcurrentDictionary()
    fun x -> cache.GetOrAdd(x, lazy f x).Value

let memoiseRec f =
    let cache = ConcurrentDictionary()
    let rec f' x = cache.GetOrAdd(x, lazy (f f' x)).Value
    f'


module Input =
    let paragraphs file = System.IO.File.ReadAllText(file).Split("\n\n")
    let lines file = System.IO.File.ReadAllLines(file)
    let grid file = System.IO.File.ReadAllLines(file) |> array2D
    
[<StructuredFormatDisplay("({x}, {y})")>]
[<Struct>]
type Point =
    { x: int; y: int }
    override this.ToString() = $"({this.x}, {this.y})"
    static member ofTuple (x: int * int) = { x = fst x; y = snd x }
    static member (+) (p1: Point, p2: Point) = { x = p1.x + p2.x; y = p1.y + p2.y }
    static member (-) (p1: Point, p2: Point) = { x = p1.x - p2.x; y = p1.y - p2.y }
    static member (*) (p: Point, scalar: int) = { x = p.x * scalar; y = p.y * scalar }
    static member (*) (scalar: int, p: Point) = { x = p.x * scalar; y = p.y * scalar }

let inline (!@) (x, y) = { x = x; y = y }

type RingBuffer2D<'T>(items : 'T[,]) =
    let leni = items.GetLength(0)
    let lenj = items.GetLength(1)
    let _items = Array2D.copy items
    member _.Item
        with get(i, j) =
            _items.[i % leni, j % lenj]
        and set (i, j) value =
            _items.[i % leni, j % lenj] <- value
            
let numbers = Array2D.init 4 5 (fun x y -> x * y)
let numberRing = RingBuffer2D(numbers)


// 0 0 -> 0
// 0 1 -> 0
// ...
// 1 1 -> 1
// 1 2 -> 2
// ..
// 9 8 -> 3
// 9 9 -> 4
// for i in 0..9 do
//     for j in 0..9 do
//         printfn "%i %i -> %A" i j (numberRing.[i,j])

let manhattan a b =
    abs(a.x - b.x) + abs(a.y - b.y)

let rec rep n (item: string) =
    match n with
    | 0 -> item
    | _ -> rep (n - 1) (item + item)

let repStr (str: string) (n: int) : string =
    if n <= 0 then ""
    else String.replicate n str

let iterN n fn =
    let rec iter fn' n' = if n' = 0 then fn' else iter (fn >> fn') (n' - 1)
    iter id n

let isEven n = n % 2 = 0
let isOdd n = n % 2 <> 0

let digitToInt (c: char) =
    if System.Char.IsAsciiDigit c then int c - int '0' else failwith "Input val is not digit char" 

let intToDigit (i: int) =
    int '0' + i |> char

let digitsToInt digits = List.fold (fun acc d -> acc * 10 + d) 0 digits
let digitsToInt64 digits = List.fold (fun acc d -> acc * 10L + d) 0L digits

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
    
let move dir dist (pos: Point) =
    match dir with
    | Up -> pos + { x = - dist; y = 0 }
    | Do -> pos + { x = + dist; y = 0 }
    | Le -> pos + { x = 0; y = - dist }
    | Ri -> pos + { x = 0; y = + dist }
    
let move8 dir dist pos =
    match dir with
    | Nrth -> pos |> move Up dist
    | Sout -> pos |> move Do dist
    | West -> pos |> move Le dist
    | East -> pos |> move Ri dist
    | NoWe -> pos |> move Up dist |> move Le dist
    | NoEa -> pos |> move Up dist |> move Ri dist
    | SoWe -> pos |> move Do dist |> move Le dist
    | SoEa -> pos |> move Do dist |> move Ri dist

let neighbours8 pos = dirs8 |> List.map (fun dir -> move8 dir 1 pos)
let neighbours4 pos = dirs |> List.map (fun dir -> move dir 1 pos)

let digitAt (pos: Point) (arr: char array2d) : int = digitToInt arr[pos.x, pos.y]
let charAt (pos: Point) (arr: char array2d) : char = arr[pos.x, pos.y]

let inbounds (map: char array2d) (pos: Point) =
    pos.x >= map.GetLowerBound 0 && pos.x <= map.GetUpperBound 0 &&
    pos.y >= map.GetLowerBound 0 && pos.y <= map.GetUpperBound 0

let testEach dirs testFn = List.iter testFn dirs

let flattenArray2D (arr: 'T[,]) =
    seq {
        for i in 0 .. Array2D.length1 arr - 1 do
            for j in 0 .. Array2D.length2 arr - 1 do
                yield arr[i, j]
    }

let flattenWithIndices (arr: 'T[,]) =
    seq {
        for i in 0 .. Array2D.length1 arr - 1 do
            for j in 0 .. Array2D.length2 arr - 1 do
                yield (i, j, arr[i, j])
    }

let findIndicesOf (char: char) (arr: char[,]) =
    flattenWithIndices arr
    |> Seq.filter (fun (_, _, value) -> value = char) 
    |> Seq.map (fun (i, j, _) -> (i, j))
    |> Seq.toList

let findChars c (str: string) =
    [ for i, x in Seq.indexed str do if x = c then i ]

let rec comb n l =
    match n, l with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, (x :: xs) -> List.map ((@) [ x ]) (comb (k - 1) xs) @ comb k xs


let printCharMap spacing (map: char array2d) =
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

let printIntMap spacing (map: int array2d) =
    for row in 0 .. Array2D.length1 map - 1 do
        for col in 0 .. Array2D.length2 map - 1 do
            let spacing' = rep spacing " "
            printf $"%i{map[row, col]}{spacing'}"
        printfn ""
        
let printInt64Map spacing (map: int64 array2d) =
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

module Search =
    let fillRegion (map: char array2d) (start: Point) (fn: Point -> char array2d -> Point-> bool): Point seq =
        let visited = HashSet<Point>()
    
        let rec step (pos: Point) =
            seq {
                if visited.Contains(pos) then
                    ()
                else
                    visited.Add(pos) |> ignore
                    yield pos
    
                    let neighbours =
                        dirs
                        |> List.map (fun dir -> move dir 1 pos)
                        |> List.filter (fn pos map)
    
                    for neighbour in neighbours do
                        yield! step neighbour
            }
    
        step start

    let dijkstra (grid: char array2d) (start: Point) (goal: Point)=
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
                        let priority = cost'
                        frontier.Enqueue((next, dir), priority)
                        from[next] <- Some(current)
        from, cost
    
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
        
    let traverse (map: char array2d) (start: Point) (fn: Point -> char array2d -> Point-> bool)=
        let visited = HashSet<Point>()

        let rec loop pos path = seq {
            let height = digitAt pos map
            let path' = (pos, height) :: path
            visited.Add(pos) |> ignore
            
            if height = 9 then yield path'
            
            let neighbours =
                dirs
                |> List.map (fun dir -> move dir 1 pos)
                |> List.filter (fun pos' -> inbounds map pos' && fn pos map pos')

            for neighbor in neighbours do
                yield! loop neighbor path'
        }

        loop start []
        
    let allPaths branchCond stopCond map start  =
        let rec dfs (pos: Point) (visited: Point Set) : Point list seq =
            let visited' = visited.Add(pos)

            let neighbours =
                dirs
                |> List.map (fun dir -> move dir 1 pos)
                |> List.filter (fun pos' -> branchCond pos map pos' && visited.Contains(pos') |> not)

            if neighbours.IsEmpty || stopCond pos map then
                // end of a path
                seq { yield [ pos ] }
            else
                neighbours
                |> Seq.collect (fun n -> dfs n visited' |> Seq.map (fun path -> pos :: path))

        dfs start Set.empty        
    
    let allPaths16 branchCond stopCond costFn map start state  =
        let rec dfs (pos: Point) (visited: Point Set) cost state =
            let visited' = visited.Add(pos)

            let neighbours =
                dirs
                |> List.map (fun dir -> dir, move dir 1 pos)
                |> List.filter (fun (_, pos') -> branchCond pos map pos' && visited.Contains(pos') |> not)

            if neighbours.IsEmpty || stopCond pos map visited then
                // end of a path
                seq { yield ([ pos ], cost) }
            else
                neighbours
                |> Seq.collect (fun (dir, pos') ->
                    let cost' = cost + costFn pos pos' state dir
                    (dfs pos' visited' cost' dir)
                    |> Seq.map (fun (path, cost) -> (pos :: path, cost)))

        dfs start Set.empty 0 state
    
    let aStar18 (grid: char array2d) (start: Point) (goal: Point) (heuristic: Point -> Point -> int) =
        // TODO: add branch, stop, and cost functions as args, then move to utils
        let frontier = PriorityQueue<Point, int>()
        frontier.Enqueue(start, 0)
        let from = Dictionary<Point, Point option>()
        let cost = Dictionary<Point, int>()
        from.Add(start, None)
        cost.Add(start, 0)
        let mutable Break = false
        
        while frontier.Count > 0 && not Break do
            let pos = frontier.Dequeue()
            
            if pos = goal then
                printfn "REACHED THE BLOODY GOAL INNIT"
                Break <- true
            else
                let neighbours =
                    dirs
                    |> List.map (fun dir -> dir, move dir 1 pos)
                    |> List.filter (fun (_, pos') -> inbounds grid pos' && charAt pos' grid = '.')

                for _, pos' in neighbours do
                    let cost' = cost[pos] + 1
                    if cost.ContainsKey(pos') |> not || cost' < cost[pos'] then
                        cost[pos'] <- cost'
                        let priority = cost' + heuristic pos' goal
                        frontier.Enqueue(pos', priority)
                        from[pos'] <- Some(pos)
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


module Validation =
    let showWarning message =
        let originalColor = System.Console.ForegroundColor
        System.Console.ForegroundColor <- System.ConsoleColor.Yellow
        printfn $"WARNING: %s{message}"
        System.Console.ForegroundColor <- originalColor

    let ensure statement =
        if statement then () else showWarning "ensure failed"
        
    ensure (9 = 0)

    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Linq.RuntimeHelpers

    /// The ensure function
    let ensure2 (expr: bool Expr) =
        // Evaluate the expression
        let isTrue = LeafExpressionConverter.EvaluateQuotation expr :?> bool
        if not isTrue then
            // Get the textual representation of the expression
            let exprString = expr.ToString()
            printfn "Warning: Ensure failed for expression: %s" exprString

    // Example usage
    let x = 5
    let y = 10
    ensure2 <@ x + y = 20 @>  // Outputs: Warning: Ensure failed for expression: x + y = 20
    ensure2 <@ x < y @>       // No output because the condition is true

module Seq =
  let countWhere predicate items =
      // items |> Seq.filter predicate |> Seq.length
    items |> Seq.sumBy (fun x -> if predicate x then 1 else 0)

module List =
  let countWhere predicate items =
      items |> List.filter predicate |> List.length

module Array =
  let countWhere predicate items =
      items |> Array.filter predicate |> Array.length

module Dial =
    let rotate_dial_n max_n position (dir: char, n: int) =
        let limit = max_n + 1
        match dir with
        | 'R' -> (position + n) % limit
        | _ -> (limit + ((position - n) % limit)) % limit