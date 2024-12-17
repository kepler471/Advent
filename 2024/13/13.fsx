#load "../utils.fs"
#r "nuget: FSharp.Stats"

open System
open FSharp.Stats.Algebra
open FSharp.Stats.Distributions.Frequency
open _2024.utils
open FSharp.Stats.Algebra.LinearAlgebra
open FSharp.Stats
open System.Text.RegularExpressions

let linalg = FSharp.Stats.Algebra.LinearAlgebra

let rawText = System.IO.File.ReadAllText("2024/13/test")

// Function to parse a line and extract numbers
let parseLine (line: string) =
    let pattern = @"[XY][+=](\d+)"
    Regex.Matches(line, pattern)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> int (m.Groups.[1].Value))
    |> Seq.toList

// Function to parse a section
let parseSection (lines: string list) =
    let buttonA = parseLine lines.[0] |> List.map float
    let buttonB = parseLine lines.[1] |> List.map float
    let prize = parseLine lines.[2] |> List.map float
    matrix [ [buttonA[0]; buttonB[0]]; [buttonA[1]; buttonB[1]] ], matrix [ [prize[0]]; [prize[1]] ]

// Split the input into sections and parse each section
let parseInput (input: string) =
    input.Split("\n\n", System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun section -> section.Split('\n') |> Array.toList)
    |> Array.map parseSection
    |> Array.toList

// Run the parser
let parsedData = parseInput rawText
(fst parsedData[0]).Columns((0, 1))
let sol = SolveLinearSystems <|| parsedData[0]
sol[1,0]

let isInteger a = abs (a % 1.0) < 0.001
(null, 12)
let solutions data =
    data
    |> List.map (fun (A, X) -> A, X, Determinant A)
    |> List.map (fun (A, X, D) ->
        match D with
        | 0.0 ->
            // Case 2: A is singular (detA = 0)
            printfn "Matrix A is singular. Exploring solutions..."
            
            // Check if B is in the column space of A
            let pseudoSolution = SolveLinearSystems (A.Transpose * A) (A.Transpose * X)
            let reconstructedX = A * pseudoSolution

            if not (reconstructedX = X) then
                printfn "No solutions: X is not in the column space of A."
                (0.0, 0.0, 0.0)
            else
                let particularSolution = pseudoSolution
                let ns = LinearAlgebra.nullspace 0.01
                let nullSpace = ns A
                
                if nullSpace.NumCols = 0 then
                    // No null space; there is exactly one solution
                    let m, n = particularSolution[0, 0], particularSolution[1, 0]
                    if m % 1.0 = 0.0 && n % 1.0 = 0.0 then
                        printfn "Single integer solution found: m = %d, n = %d" (int m) (int n)
                        (m, n, 3.0 * m + n)
                    else
                        printfn "No integer solution exists."
                        (0.0, 0.0, 0.0)
                else
                    // Null space exists; parameterize solutions
                    let nullVector = nullSpace.Columns((0, 1)) // Take the first null space vector
                    printfn "Infinite solutions exist. Exploring integer solutions..."
                
                    // Parameterize solutions as m = m_p + k * null_m, n = n_p + k * null_n
                    let m_p, n_p = particularSolution[0, 0], particularSolution[1, 0]
                    let null_m, null_n = nullVector[0, 0], nullVector[1, 0]
                
                    // Generate integer solutions for a range of k
                    let solutions =
                        [ for k in 1 .. 10 do // Adjust the range as needed
                            let m = m_p + float k * null_m
                            let n = n_p + float k * null_n
                            if m % 1.0 = 0.0 && n % 1.0 = 0.0 then
                                yield (int m, int n) ]
                
                    if solutions.IsEmpty then
                        printfn "No integer solutions exist in the explored range."
                        (0.0, 0.0, 0.0)
                    else
                        printfn "Integer solutions found: %A" solutions
                        (m_p, n_p, 3.0 * m_p + n_p)
        | _ ->
            let solution = SolveLinearSystems A X
            let m, n = solution[0, 0], solution[1, 0]
            (m, n, 3.0 * m + n))
//
// let solutions' =
//     parsedData
//     |> List.map (fun (A, X) -> A, X, Determinant A)
//     |> List.map (fun (A, X, D) ->
//         match D with
//         | 0.0 ->
//             (0.0, 0.0, 0.0)
//         | _ ->
//             // let solution = SolveLinearSystems A X
//             // let m, n = solution[0, 0], solution[1, 0]
//             let ax, ay, bx, by = fl A[0, 0], uint64 A[1, 0], uint64 A[0, 1], uint64 A[1, 1]
//             let x, y = uint64 X[0, 0] + 10000000000000UL, uint64 X[1, 0] + 10000000000000UL
//             let m = (y * bx - x * by) / (bx * ay - by * ax) |> uint64
//             let n = (ax * y - ay * x) / (ax * by - ay * bx) |> uint64
//             (m, n, 3UL * m + n))

let solutions' =
    parsedData
    |> List.map (fun (A, X) -> A, X, Determinant A)
    |> List.map (fun (A, X, D) ->
        match D with
        | 0.0 ->
            (0UL, 0UL, 0UL)
        | _ ->
            // let solution = SolveLinearSystems A X
            // let m, n = solution[0, 0], solution[1, 0]
            let ax, ay, bx, by = A[0, 0], A[1, 0], A[0, 1], A[1, 1]
            let x, y = X[0, 0] + 10000000000000.0, X[1, 0] + 10000000000000.0
            let m = (y * bx - x * by) / (bx * ay - by * ax) |> uint64
            let n = (ax * y - ay * x) / (ax * by - ay * bx) |> uint64
            (m, n, 3UL * m + n))

// 12321.00001 % 1.0 < 0.00001
// solutions |> List.filter isInteger |> List.sum
let sums = solutions'
sums |> List.map(fun (_, _, x) -> x) 
printfn $"%A{sums}"

let filteredSums = sums |> List.filter (fun (m, n, c) -> isInteger c)
let ans = filteredSums |> List.sumBy (fun (_, _, c) -> uint64 c)

ans
// parsedDataURGΩΩΩΩΩΩ
// |> List.map (fun (A, X) -> A, X, Determinant A)
// |> List.filter (fun (_, _, D) -> D = 0)
//
// parsedData
// |> List.map (fun (A, X) -> SolveLinearSystems A X)

// isInteger 334.4283802
// isInteger -89.47619048
