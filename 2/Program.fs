(*
--- Day 2: Password Philosophy ---

Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.

Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.

How many passwords are valid according to their policies?
*)
open System.IO

let input = File.ReadLines "input.txt"

let split (lines : string seq) =
  lines
  |> Seq.map ((String.filter (fun s -> s <> ':')) >> (fun x -> x.Split ' '))

let isValid1 ((lims,chr,pwd) : string*string*string) =
  let lim = lims.Split '-' |> Array.map int
  let count =
    pwd |> String.filter (fun x' -> x' = char(chr)) |> String.length
  count >= lim.[0] && count <= lim.[1]

let isValid2 ((lims,chr,pwd) : string*string*string) =
  let lim = lims.Split '-' |> Array.map int
  let a = pwd.[lim.[0]-1]
  let b = pwd.[lim.[1]-1]
  a <> b && (a = char(chr) || b = char(chr))

[<EntryPoint>]
let main argv =
  printfn "Part 1: %A" ( input
                         |> split
                         |> Seq.map ((fun a -> a.[0], a.[1], a.[2]) >>  isValid1)
                         |> Seq.filter (fun x -> x = true) |> Seq.length) 
  printfn "Part 2: %A" ( input
                         |> split
                         |> Seq.map ((fun a -> a.[0], a.[1], a.[2]) >>  isValid2)
                         |> Seq.filter (fun x -> x = true) |> Seq.length) 

  0 // return an integer exit code
