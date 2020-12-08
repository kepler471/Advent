(*
--- Day 4: Passport Processing ---

You arrive at the airport only to realize that you grabbed your North Pole Credentials instead of your passport. While these documents are extremely similar, North Pole Credentials aren't issued by a country and therefore aren't actually valid documentation for travel in most of the world.

It seems like you're not the only one having problems, though; a very long line has formed for the automatic passport scanners, and the delay could upset your travel itinerary.

Due to some questionable network security, you realize you might be able to solve both of these problems at the same time.

The automatic passport scanners are slow because they're having trouble detecting which batch have all required fields. The expected fields are as follows:

    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

Passport data is validated in batch files (your puzzle input). Each passport is represented as a sequence of key:value pairs separated by spaces or newlines. Passports are separated by blank lines.

Here is an example batch file containing four batch:

ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in

The first passport is valid - all eight fields are present. The second passport is invalid - it is missing hgt (the Height field).

The third passport is interesting; the only missing field is cid, so it looks like data from North Pole Credentials, not a passport at all! Surely, nobody would mind if you made the system temporarily ignore missing cid fields. Treat this "passport" as valid.

The fourth passport is missing two fields, cid and byr. Missing cid is fine, but missing any other field is not, so this passport is invalid.

According to the above rules, your improved system would report 2 valid batch.

Count the number of valid batch - those that have all required fields. Treat cid as optional. In your batch file, how many batch are valid?

*)

open System.IO
open System.Text.Json

let input = File.ReadLines "input.txt" |> Seq.toList
let example = File.ReadLines "example.txt" |> Seq.toList

type Passport = {
  byr: string
  iyr: string
  eyr: string
  hgt: string
  hcl: string
  ecl: string
  pid: string
  cid: string }

let parseLines (input : string list) = 
  let rec parse line batch input =
    match input with
      | [] -> batch
      | [_] -> parse line (line::batch) []
      | ""::t -> parse "" (line::batch) t
      | h::t -> parse (line + h + " ") batch t
  parse "" [] input
    
let decompose (line : string) =
  line |> Seq.map (fun c -> string(c)) |> Seq.toList

let rec jsonify (line : string list) (json : string list) =
  match line with
    | [] -> json |> List.rev |> List.reduce (+)
    | [_] -> jsonify [] ("\"}" :: json @ ["{\""])
    | ":"::t -> jsonify t ("\":\""::json)
    | " "::t -> jsonify t ("\", \""::json)
    | h::t -> jsonify t (h::json)

let rec mapify (line : string list) (m : string list) =
  match line with
    | [] -> m |> List.rev |> List.reduce (+)
    | [" "] -> mapify [] m
    | ":"::t -> mapify t (" "::m)
    | h::t -> mapify t (h::m)

let prepstring = example |> parseLines |> List.head |> decompose |> mapify <| []
let getmap = prepstring.Split ' ' |> Array.toList


let parseJson (json : string) =
  try
    let (passport : Passport) = JsonSerializer.Deserialize json
    Some(passport)
  with :? System.Text.Json.JsonException -> None

let countValid (list : Passport option list) =
  list |> List.filter (fun p -> p <> None) |> List.length

let batchToPassports (list : string list) =
  list |> parseLines |> List.map ( decompose >> (fun l -> jsonify l []) >> parseJson )  

let check (p : Passport) =
  let fields = [p.byr; p.iyr; p.eyr; p.hgt; p.hcl; p.ecl; p.pid]
  match fields |> List.filter (fun x -> x |> isNull) |> List.length with
    | 0 -> true
    | _ -> false


// input |> parseLines |> basicCheck |> List.length = 181!!!! still short by one?
let basicCheck (lst : string list) =
  let fields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
  let tryall (str : string) = fields |> List.filter (fun a -> str.Contains a |> not)
  lst |> List.filter (tryall >> List.isEmpty)

let getByName s (p : Passport option) =
  typeof<Passport>.GetProperties()
  |> Array.tryFind (fun t -> t.Name = s)
  |> Option.map (fun pi -> pi.GetValue p.Value)

let buildPassport (line : string) = {
  byr = "aslkj"
  iyr = "aslkj"
  eyr = "aslkj"
  hgt = "aslkj"
  hcl = "aslkj"
  ecl = "aslkj"
  pid = "aslkj"
  cid = "aslkj"
}

let ex1 = batchToPassports example
let passports = batchToPassports input
//let xxx = passports.Head.Value.
let d =
  Map.ofList 

[<EntryPoint>]
let main _ =
  ex1 |> List.iter (printfn "Examples: %A")
  printfn "Part 1 Count Examples. %A" (countValid ex1)
  printfn "Part 1. %A" (countValid passports)
  printfn "Part 1 again. %A" (passports |> List.filter (fun x -> check x.Value) |> List.length)
//  passports |> List.iter (printfn "%A")
  0 // return an integer exit code
