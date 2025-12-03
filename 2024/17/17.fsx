#load "../utils.fs"

open Microsoft.FSharp.Core
open Microsoft.VisualBasic

type Chrono(a: uint64, b: uint64, c: uint64, prog: (int * int) list) =
    let mutable a = a
    let mutable b = b
    let mutable c = c
    let mutable ptr = 0UL
    let mutable output: uint64 list = []
    
    member this.A with get() = a and set(value) = a <- value
    member this.B with get() = b and set(value) = b <- value
    member this.C with get() = c and set(value) = c <- value
    member this.Ptr with get() = ptr
    member this.Output with get() = output
    member this.Prog with get() = prog
    
    member this.move () = ptr <- ptr + 1UL
    
    member this.step () =
        let opcode, operand = prog[int ptr]
        match opcode with
        | 0 -> this.adv operand
        | 1 -> this.bxl operand
        | 2 -> this.bst operand
        | 3 -> this.jnz operand
        | 4 -> this.bxc operand
        | 5 -> this.out operand
        | 6 -> this.bdv operand
        | 7 -> this.cdv operand
        | _ -> failwith "should not have reached this opcode"
        
    member this.run () =
        while int ptr < List.length prog do
            this.step()
    
    member this.literalOperand (operand): uint64 =
        uint64 operand
    
    member this.comboOperand (operand): uint64 =
        match operand with
        | 0 | 1 | 2 | 3 -> uint64 operand
        | 4 -> a
        | 5 -> b
        | 6 -> c
        | _ -> failwith "UNDEFINED"
    
    member this.adv (operand) =
        let power = this.comboOperand operand
        a <- a / pown 2UL (int power)
        this.move()
    
    member this.bxl operand =
        b <- b ^^^ this.literalOperand operand
        this.move()

    member this.bst operand =
        b <- this.comboOperand operand % 8UL
        this.move()

    member this.jnz operand =
        if a <> 0UL then
            ptr <- (this.literalOperand operand) / 2UL
        else
            this.move()

    member this.bxc operand =
        b <- b ^^^ c
        this.move()

    member this.out operand =
        let result = this.comboOperand operand % 8UL
        output <- output @ [ result ]
        this.move()
        
    member this.bdv (operand) =
        let power = this.comboOperand operand
        b <- a / pown 2UL (int power)
        this.move()
    
    member this.cdv (operand) =
        let power = this.comboOperand operand
        c <- a / pown 2UL (int power)
        this.move()


let chronoFromSpec A B C (prog: string) =
    let parseProgram str = List.chunkBySize 2 str |> List.map (fun x -> x[0], x[1])
    let splitProg = prog.Split(",") |> Array.map int |> Array.toList
    Chrono(A, B, C, parseProgram splitProg)

let testSpec A B C prog = 
    let testComp = chronoFromSpec A B C prog
    testComp.run()
    testComp

let (!) x = uint64 x


let chrono = chronoFromSpec !729 !0 !0 "0,1,5,4,3,0"
chrono

let program = "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0"
testSpec !729 !0 !0 "0,1,5,4,3,0" // ts final output will be 4,6,3,5,6,3,5,2,1,0.
testSpec !0 !0 !9 "2,6" // would set register B to 1.
testSpec !10 !0 !0 "5,0,5,1,5,4" // would output 0,1,2.
testSpec !2024 !0! 0 "0,1,5,4,3,0" // would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
testSpec !0 !29 !0 "1,7" // would set register B to 26.
testSpec !0 !2024 !43690 "4,0" // would set register B to 44354.
testSpec !51571418 !0 !0 "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0" // would output 4,0,4,7,1,2,7,1,6
testSpec !51571418 !0 !0 "2,4,1,1,7,5,0,3,1,4,4,5,5,5" // would output 4
let result = testSpec !51571418 !0 !0 "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0"
result.Output |> List.map string |> String.concat ","
result.Output |> List.toArray |> Array.map int
result.Prog

let digits = [ for i in 0 .. 15 -> i ]
let pow8s = [ for i in digits -> pown 8UL i ]

let getNLastOps n (prog: string) =
    prog
    |> _.Split(",")
    |> (Array.rev >> Array.take n)
    |> Array.map string |> Array.rev |> String.concat ","

let stackSum stack =
    stack |> List.rev |> List.mapi (fun i x -> x * pown 8 (i + 1)) |> List.sum

let fn1 i options prog =
    let j = !(i + stackSum options)
    let res = testSpec j !0 !0 prog
    let output = res.Output |> List.map string |> String.concat ","
    let progTail = getNLastOps res.Output.Length prog
    output = progTail


let search (prog: string) =
    let rec step (options: int list) (digits': int list) =
        match digits' with
        | [] -> List.rev options
        | _ :: t ->
            printfn $"Options: %A{List.rev options}"
            printfn $"{[0..7] |> List.filter (fun i -> fn1 i (List.rev options) prog)}"
            
            [0..7]
            |> List.filter (fun i -> fn1 i (List.rev options) prog)
            |> List.collect (fun x -> step (x :: options) t)
    step [] digits
    
let y = search program
y
// TODO: convert some of these output from octal to decimal

[0..7] |> List.filter (fun i -> fn1 i [5; 6; 0; 0] program)

testSpec 5614132756UL !0 !0 program 
testSpec 6572314165UL !0 !0 program 
testSpec 5600UL !0 !0 program 
testSpec 46UL !0 !0 program 


testSpec !2024 !0 !0 "0,3,5,4,3,0"
(testSpec !117440 !0 !0 "0,3,5,4,3,0").Output |> List.map string |> String.concat ","

let mutable i = 1_000_000
let mutable flag = true
let program = "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0"
while flag do
    let res = testSpec i 0 0 program
    let output = res.Output |> List.map string |> String.concat ","
    if i % 1_000_000 = 0 then
        printfn $"i: {i} -> output: {output}"
    if program = output then
        printfn $"Program matches OUTPUT: {program} = {output}"
        flag <- false
    i <- i + 1
    
testSpec !5 !0 !0 "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0"

5 * pown 8 0
0o5

6 * pown 8 0 +
5 * pown 8 1
0o56
Conversion.Oct 46

0 * pown 8 0 + 
6 * pown 8 1 +
5 * pown 8 2
Conversion.Oct 368
0o560

0 * pown 8 0 + 
0 * pown 8 1 +
6 * pown 8 2 +
5 * pown 8 3
0o5600
Conversion.Oct 2944 

[ for i in digits -> List.take (i + 1) pow8s ]


let fn scalars =
    let rec fn' scalars' acc =
        match scalars' with
        | [] -> 0
        | [ _ ] -> acc
        | h :: t ->
            let n = List.length t + 1
            fn' t (acc + h * pown 8 n)
    fn' scalars 0

let finder stack =
    let range = { 0..7 }
    [for i in range do
         let output = testSpec (!i + !(fn stack)) !0 !0 program |> _.Output |> List.map string |> String.concat ","
         let matcher =
          program |> _.Split(",")
          |> (Array.rev >> Array.take (List.length stack))
          |> Array.map string |> String.concat ","
         if matcher = output then
             yield i ]

fn [5; 6; 0]
finder [0]



getNLastOps 4 program


let mutable flag = true
let mutable stack: int list = []
let mutable options: int list list = []


for d in digits do
    for op in options.Head do
        let search =
             [ for i in 0 .. 7 do
                 let j = !(i + stackSum stack)
                 let res = testSpec j !0 !0 program
                 if res.Output |> List.map string |> String.concat "," = getNLastOps (res.Output.Length) program then
                     yield i ]
        options <- search :: options



[ for i in 0 .. 7 do
     let j = !(i)
     let res = testSpec j !0 !0 program
     j, res.Output |> List.map string |> String.concat "," ]

[ for i in 0 .. 7 do
     let j = !(i + 5 * pown 8 1)
     let res = testSpec j !0 !0 program
     j, res.Output |> List.map string |> String.concat "," ]

[ for i in 0 .. 7 do
     let j = !(i + 5 * pown 8 2 + 6 * pown 8 1)
     let res = testSpec j !0 !0 program
     j, res.Output |> List.map string |> String.concat "," ]

[ for i in 0 .. 7 do
     let powSum = stackSum [5; 6; 0; 0]
     // let powSum = 5 * pown 8 3 + 6 * pown 8 2 + 0 * pown 8 1
     let j = !(i + powSum)
     let res = testSpec j !0 !0 program
     if res.Output |> List.map string |> String.concat "," = getNLastOps (res.Output.Length) program then
         yield i ]

[for i in 0 .. 1000000 do
     let res = testSpec (i) 0 0 "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0"
     i, res.Output |> List.map string |> String.concat "," ]
|> List.filter (fun (a, b) -> b = "5,5,5,3,0")



let x = 3

testSpec 1618481116086278UL 0UL 0UL "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0"
testSpec 23649UL 0UL 0UL "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0"

testSpec (uint64 (5 + 46 + 368 + 2944)) 0UL 0UL "2,4,1,1,7,5,0,3,1,4,4,5,5,5,3,0"
pown 8 4
0o72

0o26
Conversion.Oct 22