let input =
    System.IO.File.ReadAllLines "inputs/15.txt"
    |> array2D
    |> Array2D.map (string >> int)

System.Collections