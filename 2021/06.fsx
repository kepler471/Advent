let input = 
    let raw = 
        System.IO.File.ReadLines "2021/inputs/06.txt"
        |> Seq.head
    raw.Split [|','|] |> Seq.map (fun x -> int x)

type population = 
    { mutable p0: int
      mutable p1: int
      mutable p2: int
      mutable p3: int
      mutable p4: int
      mutable p5: int
      mutable p6: int
      mutable p7: int
      mutable p8: int } 
    
    member x.age =     
        x.p0 <- x.p1
        x.p1 <- x.p2
        x.p2 <- x.p3
        x.p3 <- x.p4
        x.p4 <- x.p5
        x.p5 <- x.p6
        x.p6 <- x.p7 + x.p0
        x.p7 <- x.p8
        x.p8 <- x.p0 

    static member fromSeq input = 
        let l = input |> Seq.countBy id |> Seq.map (fun (_, x) -> x) |> Seq.toList
        { p0=l.[0]
          p1=l.[1]
          p2=l.[2]
          p3=l.[3]
          p4=l.[4]
          p5=l.[5]
          p6=l.[6]
          p7=l.[7]
          p8=l.[8] }

let a = { p0=2; p1=0; p2=1; p3=2; p4=3; p5=4; p6=5; p7=6; p8=7 }

population.fromSeq input
let rec iterate n f =
    match n with
    | 0 -> id
    | _ -> f >> iterate (n-1) f

type pop = 
    { mutable p0: int; p1; p3 } 

// type pop2 = Pop2 of mutable int * int * int

type pop3 = Array of int


seq {1;1;1;2;3;4;4;4;5;5;6;7;6;6;} |> Seq.countBy id