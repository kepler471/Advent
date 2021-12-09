let input = 
    System.IO.File.ReadAllLines "inputs/08.txt"
    |> Array.map (fun x -> x.Split [|' '|])

let outs = 
    input
    |> Array.map (fun x -> x.[11..14])

let is1478 =
    (fun (x: string) -> x.Length = 2 || x.Length = 3 || x.Length = 4 || x.Length = 7 )

let only1478 = 
    outs |> Array.map (Array.filter is1478)

let flatten s = 
    seq { for i in s do
            for j in i do yield j }

flatten only1478 |> Seq.length

let count1478 list = 
    list 
    |> Array.map String.length
    // |> 

type Digit = 
    | One   of Set<char>
    | Two   of Set<char>
    | Three of Set<char>
    | Four  of Set<char>
    | Five  of Set<char>
    | Six   of Set<char>
    | Seven of Set<char>
    | Eight of Set<char>
    | Nine  of Set<char>
    | Zero  of Set<char>


let setLength s =
    s |> Set.toList |> List.length











type Digits = 
    { One   : Set<char>
      Two   : Set<char>
      Three : Set<char>
      Four  : Set<char>
      Five  : Set<char>
      Six   : Set<char>
      Seven : Set<char>
      Eight : Set<char>
      Nine  : Set<char>
      Zero  : Set<char> }

let stringToDigit s (d: Digits) =
    let chars = Set s
    match String.length s with
    | 2 -> { d with One = chars }
    | 3 -> { d with Seven = chars }
    | 4 -> { d with Four = chars }
    | 5 -> 
        match d with 
        | d when chars.IsProperSupersetOf d.One -> { d with Three = chars }
        | d when setLength(chars - d.Four) > 2  -> { d with Two = chars }
        | _                                     -> { d with Five = chars }
    | 6 -> 
        match d with 
        | d when chars.IsProperSupersetOf d.Four  -> { d with Nine = chars }
        | d when chars.IsProperSupersetOf d.Seven -> { d with Zero = chars }
        | _                                       -> { d with Six = chars }
    | 7 | _ -> { d with Eight = chars }









let abc = Set("ce")
(Set("abcde")).IsProperSupersetOf abc
Set("bdcf") - Set("cf")
Set("acdeg") - Set("bdcf")
Set("abdfg") - Set("bdcf") |> Set.toList |> List.length

        // if chars.IsProperSupersetOf d.One 
        //     then { d with Three = chars }
        // elif setLength(chars - d.Four) > 2 
        //     then { d with Two = chars }
        // else 
        //     { d with Five = chars }
        // if chars.IsProperSupersetOf d.Four then { d with Nine = chars }
        // elif chars.IsProperSupersetOf d.Seven then { d with Zero = chars }
        // else { d with Six = chars }


// Array.sor
// stringToDigit "abcsdfsdfjsd"
// Set.union Set("abc") Set("bce")
// Set("abc") - Set("bce")
// [ for i in input.[0].[11..14] -> i]
// One(Set "abc") + Two(Set "abc")

// let mydigits1 = {One = Set "abc"; Zero = Set "bcd"} 
// mydigits1.One <- Set "ghj"

// let mydigits = { One = Set "abc"  }

// input.[0].[11..14]

// Set("abcde")
// Set("ef") = Set("fe")