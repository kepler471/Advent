#load "../utils.fs"

open _2025.utils 

let printing_department = Input.grid "04/input"
printCharMap 0 printing_department
let rollPositions = findIndicesOf '@' printing_department |> List.map (!@)

let countNeighbours state roll =
    let filter x = inbounds state x && charAt x state = '@'   
    roll
    |> neighbours8
    |> List.filter filter
    |> List.length

rollPositions |> List.map (countNeighbours printing_department) |> List.countBy ((>) 4)

let removeRolls (map: char array2d) recursive =
    let rec remove state =
        let rolls = findIndicesOf '@' state |> List.map (!@)
        let removals = rolls |> List.filter (fun x -> countNeighbours state x < 4)
        if List.length removals = 0 then
            state
        else
            List.iter (setCharAt state '.') removals
            remove state
    
    remove (Array2D.copy map)
    
let finalState = removeRolls printing_department
// printCharMap 0 printing_department
// printCharMap 0 finalState
List.length rollPositions - List.length (findIndicesOf '@' finalState)
