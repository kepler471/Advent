let template = "OFSVVSFOCBNONHKFHNPK"

let rules = 
    [|"HN", "C";
      "VB", "K";
      "PF", "C";
      "BO", "F";
      "PB", "F";
      "OH", "H";
      "OB", "N";
      "PN", "O";
      "KO", "V";
      "CK", "V";
      "FP", "H";
      "PC", "V";
      "PP", "N";
      "FN", "N";
      "CC", "F";
      "FC", "N";
      "BP", "N";
      "SH", "F";
      "NS", "V";
      "KK", "B";
      "HS", "C";
      "NV", "N";
      "FO", "B";
      "VO", "S";
      "KN", "F";
      "SC", "V";
      "NB", "H";
      "CH", "B";
      "SF", "V";
      "NP", "V";
      "FB", "P";
      "CV", "B";
      "PO", "P";
      "SV", "P";
      "OO", "V";
      "PS", "C";
      "CO", "N";
      "SP", "B";
      "KP", "H";
      "KH", "S";
      "KS", "S";
      "NH", "K";
      "SS", "P";
      "PV", "P";
      "KV", "V";
      "ON", "N";
      "BS", "C";
      "HP", "K";
      "SB", "P";
      "VC", "B";
      "HB", "N";
      "FS", "V";
      "VP", "K";
      "BB", "N";
      "FK", "S";
      "CS", "P";
      "SO", "F";
      "HF", "F";
      "VV", "C";
      "BC", "S";
      "SN", "K";
      "KB", "H";
      "BN", "H";
      "HO", "S";
      "KC", "F";
      "CP", "S";
      "HC", "S";
      "OS", "K";
      "NK", "N";
      "BF", "S";
      "VN", "B";
      "SK", "K";
      "HV", "B";
      "KF", "H";
      "FV", "B";
      "VF", "H";
      "BH", "S";
      "NN", "O";
      "HH", "K";
      "CN", "H";
      "PH", "V";
      "NF", "S";
      "OV", "P";
      "OC", "V";
      "OK", "H";
      "OF", "H";
      "HK", "N";
      "FH", "P";
      "BK", "N";
      "VS", "H";
      "NO", "V";
      "VK", "K";
      "CF", "N";
      "CB", "N";
      "NC", "K";
      "PK", "B";
      "VH", "F";
      "FF", "C";
      "BV", "P";
      "OP", "K"|]
    |> Map.ofArray

let pairs(poly : string) : string [] = 
    poly.ToCharArray()
    |> Array.pairwise
    |> Array.map(fun (a, b) -> string a + string b)

let pairCount(poly : string) = 
    let mutable counts = 
        rules.Keys
        |> Seq.map(fun x -> (x, 0UL))
        |> Map.ofSeq
    poly
    |> pairs
    |> Array.countBy id
    |> Array.map(fun (p, n) -> p, uint64 n)
    |> Array.fold (fun m (p, n) -> 
           m |> Map.change p (function 
                    | Some(a) -> Some(a + n)
                    | None -> None)) counts

let grow (rules : Map<string, string>) (polymerPairs : Map<string, uint64>) = 
    polymerPairs
    |> Map.toArray
    |> Array.fold (fun (m : Map<string, uint64>) (p, n) : string * uint64 -> 
           let p1, p2 = string p.[0] + rules.[p], rules.[p] + string p.[1]
           m
           |> Map.change p (function 
                  | Some(a) -> Some(a - n)
                  | None -> None)
           |> Map.change p1 (function 
                  | Some(a) -> Some(a + n)
                  | None -> None)
           |> Map.change p2 (function 
                  | Some(a) -> Some(a + n)
                  | None -> None)) polymerPairs

let letterCount (poly : string) (pairs : Map<string, uint64>) = 
    let (first, last) = Seq.head poly, Seq.last poly
    pairs
    |> Map.toArray
    |> Array.fold (fun acc (p, n) -> (p.[1], n) :: (p.[0], n) :: acc) []
    |> List.groupBy(fun (p, _) -> p)
    |> List.map(fun (p, pn) -> p, pn |> List.sumBy snd)
    |> List.map(fun (p, n) -> 
           if p = first || p = last then string p, (n + 1UL) / 2UL
           else string p, n / 2UL)
    |> Map.ofList

let subtract poly = 
    poly
    |> Map.toArray
    |> Seq.countBy id
    |> (fun x -> 
    let min, max = Seq.minBy snd x, Seq.maxBy snd x
    max, min, snd max - snd min)

[1..10]
|> Seq.fold (fun acc x -> grow rules acc) (pairCount template)
|> letterCount template
|> Map.toSeq
|> (fun p -> Seq.maxBy snd p, Seq.minBy snd p)