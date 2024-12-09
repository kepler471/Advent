#load "../utils.fs"

open _2024.utils
open Microsoft.FSharp.Core

let diskMap = System.IO.File.ReadAllText("2024/09/test") + "0"

[<Measure>] type block

type DiskChunk =
    { file: int<block>; space: int<block> option }
    static member ofCharArray = function
        | [| a; b |] -> { file = digitToInt a * 1<block>
                          space = Some(digitToInt b * 1<block>) } 
        | _ -> failwith "Cannot make a pair from the input structure"
    member this.unpack =
        ()
    override this.ToString() =
        let space = if this.space.IsSome then this.space.Value else 0<block>
        $"DiskMapChunk {{ file = %d{this.file}; space = %d{space} }}"

let chunkDisk =
        Seq.map id >> Seq.chunkBySize 2 >> Seq.map DiskChunk.ofCharArray

let chunkedDisk = chunkDisk diskMap
let chunkedDiskWithIds =
    chunkedDisk |> Seq.toList |> List.mapi (fun i x -> (i, x))

chunkedDiskWithIds
|> List.map (snd >> _.file) |> List.sum 

string (int 4<block>)
let writeBlocks (chunks: (int * DiskChunk) list) : int =
    let writeLimit = chunks |> List.map (snd >> _.file) |> List.sum
    let rec write stack revStack acc pos lim =
        match stack with
        | _ when pos = (lim / 1<block>) -> acc
        | h :: t -> acc
    write chunks (Seq.rev chunks) 0 0 writeLimit
    
writeBlocks chunkedDiskWithIds

1<block> / 1<block>

let rec rep n (item: string) =
    match n with
    | 0 -> item
    | _ -> rep (n - 1) (item + item)
    
diskMap |> Seq.map digitToInt |> Seq.sum
rep 95112 diskMap