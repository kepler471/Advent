let input =
    System.IO.File.ReadAllLines "inputs/11.txt"
    |> array2D
    |> Array2D.map (string >> int)

type matrix = int [,]

let rec iterate n f =
    match n with
    | 0 -> id
    | _ -> f >> iterate (n-1) f

let inbounds i j (m: matrix) = 
    if m.GetLowerBound 0 <= i && i <= m.GetUpperBound 0
        && m.GetLowerBound 1 <= j && j <= m.GetUpperBound 1
    then true 
    else false

let inc m = m |> Array2D.map ((+) 1)

let splash m = 
    let flash i j (m: matrix) = 
        let iLo = if i - 1 < m.GetLowerBound 0 then 0 else i
        let iHi = if i + 1 > m.GetUpperBound 0 then m.GetUpperBound 0 else i
        let jLo = if j - 1 < m.GetLowerBound 1 then 0 else i
        let jHi = if i + 1 > m.GetUpperBound 1 then m.GetUpperBound 1 else j
        m.[iLo..iHi, jLo..jHi] <- inc(m.[iLo..iHi, jLo..jHi])
        m
    m |> Array2D.map

let zero m = m |> Array2D.map (fun x -> if x > 9 then 0 else x)

let simulate n m =
    let rec step n m = 
        match n with
        | 0 -> m
        | _ -> step (n - 1) ((inc >> splash >> zero) m)
    step n m

(Array2D.
input.[2..4,2..4] <- (inc >> inc >> inc >> inc >> inc) input.[2..4,2..4]
inc input
(inc >> inc >> inc) input