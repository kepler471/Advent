#load "../utils.fs"

open _2025.utils
open System.Collections.Generic

let commands = Input.lines "01/input" |> Seq.map (fun r -> r.[0], int r.[1..])

let rotate_dial = Dial.rotate_dial_n 99

let positions = commands |> Seq.scan rotate_dial 50

let n_zeros = positions |> Seq.countWhere ((=) 0)

// Could 'expand' the original instructions, repeat each entry
let expand (dir: char, n: int) = Seq.replicate n (dir, 1)

let n_zeros_1 =
    commands
    |> Seq.scan rotate_dial 50
    |> Seq.countWhere ((=) 0)
    
let n_zeros_0x434C49434B =
    commands
    |> Seq.collect expand
    |> Seq.scan rotate_dial 50
    |> Seq.countWhere ((=) 0)
