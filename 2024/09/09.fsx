#load "../utils.fs"

open _2024.utils
open Microsoft.FSharp.Core

let diskMap = System.IO.File.ReadAllText("2024/09/test") + "0"

type DiskChunk =
    { file: int; space: int option }
    static member ofCharArray = function
        | [| a; b |] -> { file = digitToInt a * 1
                          space = Some(digitToInt b) } 
        | _ -> failwith "Cannot make a pair from the input structure"
    member this.unpack =
        ()
    override this.ToString() =
        let space = if this.space.IsSome then this.space.Value else 0
        $"DiskMapChunk {{ file = %d{this.file}; space = %d{space} }}"

let chunkDisk =
        Seq.map id >> Seq.chunkBySize 2 >> Seq.map DiskChunk.ofCharArray

let chunkedDisk = chunkDisk diskMap
let chunkedDiskWithIds =
    chunkedDisk |> Seq.toList |> List.mapi (fun i x -> (i, x))

let writeBlocks (chunks: (int * DiskChunk) list) : int * int list * string list =
    let writeLimit = chunks |> List.map (snd >> _.file) |> List.sum
    
    let rec write (stack: (int * DiskChunk) list) (revStack: (int * DiskChunk) list) acc pos lim result oplog =
        // printfn $"%A{stack.Head} %A{revStack.Head} %A{acc} %A{pos} %A{lim}"
        // printfn "hello there"
        match stack with
        // | _ when pos = lim -> acc
        // Head Chunk is now depleted, move on to next Chunk
        | (_, { file = 0; space = Some(0) }) :: t ->
            printfn $"#### Enter Op.1. First chunk file and space is empty: %A{stack.Head}. Skip to next operation"
            write t revStack acc pos lim (99 :: result) ("Op.1" :: oplog)
        
        // | _ when (snd revStack.Head).file > 0 ->
        //     // let 
        //     write stack (revStack.Tail) acc pos lim (99 :: result) ("Op.1t" :: oplog)
        
        // Chunk has no file blocks left, fill the space blocks by depleting tail file blocks
        | (i, { file = 0; space = Some(space) }) :: t ->
            printfn $"#### Enter Op.2. First chunk file is empty, but has space:\n%A{stack.Head}"
            let chunk = { file = 0; space = Some(space - 1) }
            // let _, tailChunk = List.head revStack
            // let tailId, tailChunk =
            let newRevStack =
                if (snd revStack.Head).file > 0
                then
                    printfn $"\t- Head of rev stack file > 0. Use it:\n{revStack.Head}"
                    revStack
                else
                    printfn $"\t- Head of rev stack file = 0. Skip and use next:\n{revStack.Tail.Head}"
                    revStack.Tail
            // let newRevStack = revStack.Tail |> List.skip 1
            let tailId, tailChunk = newRevStack |> List.head
            printfn $"\t- Confirming skipped on tail and moved to:\n{newRevStack.Head}"
            let swapChunk = { file = tailChunk.file - 1; space = tailChunk.space }
            write ((i, chunk) :: t) ((tailId, swapChunk) :: newRevStack) (acc + tailId * pos) (pos + 1) lim (tailId :: result) ("Op.2" :: oplog)
        
        // Chunk has value left in the file, deplete it before filling space
        | (i, { file = data; space = Some(space) }) :: t ->
            printfn $"#### Enter Op.3. First chunk file is not empty:\n%A{stack.Head}"
            let chunk = { file = data - 1; space = Some(space) }
            write ((i, chunk) :: t) revStack (acc + i * pos) (pos + 1) lim (i :: result) ("Op.3" :: oplog)
        
        | [] -> acc,  2000 :: result, "END" :: oplog
        | _ -> failwith $"Uncaught error: %A{stack.Head} %A{revStack.Head} %A{acc} %A{pos} %A{lim}"
        // | (i, chunk) :: t when chunk.file > 0 ->  write 
        
    write chunks (List.rev chunks) 0 0 writeLimit [1000] ["BEGIN"]
    

// let rec rep n (item: string) =
//     match n with
//     | 0 -> item
//     | _ -> rep (n - 1) (item + item)
//     
// diskMap |> Seq.map digitToInt |> Seq.sum
// rep 95112 diskMap
//
// let test1: int list =
//   [0; 0; 9; 9; 8; 1; 1; 1; 8; 8; 8; 2; 8; 8; 8; 3; 3; 3; 8; 4; 4; 8; 5; 5; 5;
//    5; 8; 6; 6; 6; 6; 8; 7; 7; 7; 8; 8; 8; 8; 8; 9; 9]
//   
// val states: int list =
//   [2000; 99; 9; 9; 99; 8; 8; 8; 8; 99; 9; 7; 7; 7; 99; 9; 6; 6; 6; 6; 99; 9; 5;
//    5; 5; 5; 99; 9; 4; 4; 99; 9; 3; 3; 3; 99; 9; 9; 9; 2; 99; 9; 9; 9; 1; 1; 1;
//    99; 9; 9; 9; 0; 0; 1000]
let cnt, states, ops = writeBlocks chunkedDiskWithIds
List.zip states ops |> List.rev
