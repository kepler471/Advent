#load "../utils.fs"

open _2024.utils
open Microsoft.FSharp.Core

let diskMap = System.IO.File.ReadAllText("2024/09/input") + "0"

type DiskChunk =
    { file: int64; space: int64 option }
    static member ofCharArray = function
        | [| a; b |] -> { file = int64 (digitToInt a)
                          space = Some(digitToInt b) } 
        | _ -> failwith "Cannot make a pair from the input structure"
    member this.unpack =
        ()
    override this.ToString() =
        let space = if this.space.IsSome then this.space.Value else 0L
        $"DiskMapChunk {{ file = %d{this.file}; space = %d{space} }}"

let chunkDisk =
        Seq.map id >> Seq.chunkBySize 2 >> Seq.map DiskChunk.ofCharArray

let chunkedDisk = chunkDisk diskMap
let chunkedDiskWithIds = chunkedDisk |> Seq.mapi (fun i x -> (int64 i, x))

let writeBlocks (chunks: (int64 * DiskChunk) list) : int64 * int64 list * string list =
    let writeLimit = chunks |> List.map (snd >> _.file) |> List.sum
    
    let rec write (stack: (int64 * DiskChunk) list) (revStack: (int64 * DiskChunk) list) acc pos lim result oplog =
        match stack with
        | _ when pos = lim -> acc,  2000L :: result, "END" :: oplog
        // Head Chunk is now depleted, move on to next Chunk
        | (_, { file = 0L; space = Some(0L) }) :: t ->
            // printfn $"#### Enter Op.1. First chunk file and space is empty: %A{stack.Head}. Skip to next operation"
            write t revStack acc pos lim (99L :: result) ("Op.1" :: oplog)
        
        // Chunk has no file blocks left, fill the space blocks by depleting tail file blocks
        | (i, { file = 0L; space = Some(space) }) :: t ->
            // printfn $"#### Enter Op.2. First chunk file is empty, but has space:\n%A{stack.Head}"
            let chunk = { file = 0L; space = Some(space - 1L) }
            let newRevStack =
                if (snd revStack.Head).file > 0
                then
                    // printfn $"\t- Head of rev stack file > 0. Use it:\n{revStack.Head}"
                    revStack
                else
                    // printfn $"\t- Head of rev stack file = 0. Skip and use next:\n{revStack.Tail.Head}"
                    revStack.Tail
            let tailId, tailChunk = newRevStack |> List.head
            let swapChunk = { file = tailChunk.file - 1L; space = tailChunk.space }
            write ((i, chunk) :: t) ((tailId, swapChunk) :: newRevStack.Tail) (acc + tailId * pos) (pos + 1L) lim (tailId :: result) ("Op.2" :: oplog)
        
        // Chunk has value left in the file, deplete it before filling space
        | (i, { file = data; space = Some(space) }) :: t ->
            // printfn $"#### Enter Op.3. First chunk file is not empty:\n%A{stack.Head}"
            let chunk = { file = data - 1L; space = Some(space) }
            write ((i, chunk) :: t) revStack (acc + i * pos) (pos + 1L) lim (i :: result) ("Op.3" :: oplog)
        
        | _ -> failwith $"Uncaught error: %A{stack.Head} %A{revStack.Head} %A{acc} %A{pos} %A{lim}"
        
    write chunks (List.rev chunks) 0L 0L writeLimit [1000L] ["BEGIN"]
    
let cnt, states, ops = (Seq.toList >> writeBlocks ) chunkedDiskWithIds