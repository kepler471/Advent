#load "../utils.fs"

open Microsoft.FSharp.Core
open _2024.utils
open System

type Equation =
    { Result: int64 
      Parts: int64 list }
    
    static member parseEquation (input: string) : Equation =
        let parts = 
            input.Replace(":", "").Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int64
        { Result = parts[0]; Parts = parts[1..] |> List.ofArray}
        
let equations = System.IO.File.ReadAllLines("2024/07/input") |> Seq.map Equation.parseEquation

let ops1 = [ (+); (*) ]
let ( |~| ) (a: int64) (b: Int64) = string a + string b |> int64
let ops2 = ( |~| ) :: ops1 

let rec permutationsWithReplacement n list =
    if n = 0 then
        [[]]
    else
        list
        |> List.collect (fun x ->
            permutationsWithReplacement (n - 1) list
            |> List.map (fun perm -> x :: perm)
        )

let rec burn nums (ops: (int64 -> int64 -> int64) list) =
    match nums with
    | a :: b :: t -> burn (ops.Head a b :: t) ops.Tail 
    | [ x ] -> x
    | [] -> failwith "todo"

let trySolve (ops: (int64 -> int64 -> int64) list) (eq: Equation) =
    let opCombs = permutationsWithReplacement (eq.Parts.Length - 1) ops
    opCombs
    |> Seq.tryPick (fun opComb ->
        let res = burn eq.Parts opComb
        if eq.Result = res then Some(res) else None) 

let calculateCalibrationResult equations ops =
    equations
    |> Seq.map (trySolve ops)
    |> Seq.filter Option.isSome
    |> Seq.map _.Value
    |> Seq.sum

printfn $"Part One: %d{calculateCalibrationResult equations ops1}"
printfn $"Part Two: %d{calculateCalibrationResult equations ops2}"