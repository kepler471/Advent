#r "nuget: SixLabors.ImageSharp"
#r "nuget: SixLabors.ImageSharp.Drawing"
#load "../utils.fs"
#load "../animate.fs"

open _2025.utils
open _2025.animate
open SixLabors.ImageSharp.PixelFormats

let config = {
    scale = 8
    colorMap = function
        | '@' -> Rgba32(255uy, 100uy, 100uy)
        | '.' -> Rgba32(50uy, 50uy, 50uy)
        | _ -> Rgba32(100uy, 100uy, 255uy)
    trailColors = [
        Rgba32(255uy, 120uy, 0uy)   // age 0: deep orange (new)
        Rgba32(255uy, 140uy, 0uy)   // age 1: gold/yellow (fading)
        Rgba32(255uy, 160uy, 0uy)   // age 2: gold/yellow (fading)
        Rgba32(255uy, 180uy, 0uy)   // age 3: gold/yellow (fading)
        Rgba32(255uy, 200uy, 0uy)   // age 4: gold/yellow (fading)
        Rgba32(255uy, 220uy, 0uy)   // age 5: gold/yellow (fading)
    ]
    backgroundColor = Rgba32(0uy, 0uy, 0uy)
    renderMode = Text("Monaco", 16f)
}

let printing_department = Input.grid "04/input"
let rollPositions = findIndicesOf '@' printing_department |> List.map (!@)

let countNeighbours state roll =
    let filter x = inbounds state x && charAt x state = '@'   
    roll
    |> neighbours8
    |> List.filter filter
    |> List.length

rollPositions |> List.map (countNeighbours printing_department) |> List.countBy ((>) 4)

let removeRolls (map: char array2d) =
    let recorder = FrameRecorder(config) 
    let rec remove state =
        let rolls = findIndicesOf '@' state |> List.map (!@)
        let removals = rolls |> List.filter (fun x -> countNeighbours state x < 4)
        recorder.AddFrame(state, removals, $"Removing {List.length removals}")
        if List.length removals = 0 then
            state
        else
            List.iter (setCharAt state '.') removals
            remove state

    recorder.AddFrame(printing_department)
    let final = remove (Array2D.copy map)
    recorder.Flush()
    recorder.Save("04/evolution.gif", 10)
    final

let finalState = removeRolls printing_department
List.length rollPositions - List.length (findIndicesOf '@' finalState)
