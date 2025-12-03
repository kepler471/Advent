#load "../utils.fs"
#r "nuget: Plotly.NET.Interactive, 5.0.0"
#r "nuget: FSharp.Stats"
#r "nuget: SkiaSharp"

open System
open DynamicObj.DynObj
open FSharp.Stats
open _2024.utils
open System.Text.RegularExpressions
open Plotly.NET
open System.Collections.Generic

let arrowToDir char =
    match char with
    | '^' -> Up
    | 'v' -> Do
    | '>' -> Ri
    | _ -> Le

let paras = Input.paragraphs "2024/15/input"
let warehouse = paras[0].Split("\n") |> array2D
let moves =
    paras[1].Split("\n") |> Array.collect Array.ofSeq |> Array.map arrowToDir |> Array.toList

let interleaveColumns (array1: char[,]) (array2: char[,]) =
    // Get the dimensions of the input arrays
    let rows1, cols1 = Array2D.length1 array1, Array2D.length2 array1
    let rows2, cols2 = Array2D.length1 array2, Array2D.length2 array2

    // Ensure dimensions match for interleaving
    if rows1 <> rows2 then 
        failwith "Arrays must have the same number of rows for interleaving."

    // The number of columns in the result will be cols1 + cols2
    let result = Array2D.create rows1 (cols1 + cols2) ' '

    // Fill the result array by interleaving the columns
    for row in 0 .. rows1 - 1 do
        for col in 0 .. cols1 - 1 do
            result[row, col * 2] <- array1[row, col]       // Even-indexed columns from array1
            result[row, col * 2 + 1] <- array2[row, col]   // Odd-indexed columns from array2

    result

let warehouseLeft =
    warehouse |> Array2D.map (fun x ->
        match x with
        | 'O' -> '['
        | _ -> x)
    
let warehouseRight =
    warehouse |> Array2D.map (fun x ->
        match x with
        | 'O' -> ']'
        | '@' -> '.'
        | _ -> x)
    
let warehouse2 = interleaveColumns warehouseLeft warehouseRight
let getRobot map = findChars '@' map |> List.head |> Point.ofTuple 
let robotInit = getRobot warehouse
let robotInit2 = getRobot warehouse2
printCharMap warehouse2 0

let look (dir: Dirs) (arr: char array2d) (ij: Point) =
    let (i, j) = ij.x, ij.y

    match dir with
    // slicing a range on an array has inbuilt bounds checking
    | Up -> arr[ .. i, j] |> Array.rev |> Array.toList
    | Do -> arr[i .. , j] |> Array.toList
    | Le -> arr[i, .. j ] |> Array.rev |> Array.toList
    | Ri -> arr[i, j .. ] |> Array.toList


let setCharAt (map: char array2d) (char: char) (at: Point) =
    map[at.x, at.y] <- char 

let shiftChars (map: char array2d) n fill dir (points: Point list) =
    let chars = points |> List.map (fun x -> x, charAt x map)
    let chars' = chars |> List.map (fun (x, c) -> move dir n x, c)
    chars |> List.iter (fun (x, _) -> setCharAt map fill x)
    chars' |> List.iter (fun (x, c) -> setCharAt map c x)

exception Blocked of string
let fillRegion (map: char array2d) (start: Point) (push: Dirs) =
    let visited = HashSet<Point>()

    let rec step (pos: Point) =
        // printfn $"VISIT - {pos.x},{pos.y}"
        seq {
            if visited.Contains(pos) then
                ()
            elif charAt pos map = '#' then
                // printfn $"BOX COLLISION: {pos.x},{pos.y}"
                raise (Blocked "BLOCKED PATH: Encountered a wall")
            else
                visited.Add(pos) |> ignore
                yield pos
                let options =
                    [ push; Le; Ri ]
                    |> List.filter (fun dir ->
                        let pos' = move dir 1 pos
                        (charAt pos map = '[' && dir = Ri && charAt pos' map = ']') ||
                        (charAt pos map = ']' && dir = Le && charAt pos' map = '[') ||
                        (charAt pos map = ']' && push = Ri && charAt pos' map = '[') ||
                        (charAt pos map = '[' && push = Le && charAt pos' map = ']') ||
                        ((dir = Up || dir = Do) && charAt pos' map = '[') ||
                        ((dir = Up || dir = Do) && charAt pos' map = ']') ||
                        charAt pos' map = '#')
                // printfn $"Possible search dirs: {options}"
                let neighbours =
                    options
                    |> List.map (fun dir ->
                        move dir 1 pos)
                
                for neighbour in neighbours do
                    yield! step neighbour
        }

    try
        step start |> Seq.toList
    with
        | _ -> []
// printCharMap test6th 0
// fillRegion test6th (move Up 1 (getRobot test6th)) Up
let mutable output = ()
// TODO: Lets write out debug information and viz data to this object

let simulate (map: char array2d) (moves: Dirs list) output: char array2d =
    let map' = map |> Array2D.copy
    let rec step (moves: Dirs list) (robo: Point) =
        // printCharMap map' 0
        match moves with
        | [] -> ()
        | dir :: nextMoves ->
            // printfn $"## Robo at {robo.x},{robo.y}, moving {dir}"
            let robo' = move dir 1 robo
            let char' = charAt robo' map'
            // printfn $"## Next char will be a {char'}"
            match char' with
            | '.' ->
                // If there is clear space to move, then move
                // printfn $"#### Clear, move to space at {robo'.x},{robo'.y}"
                setCharAt map' '.' robo
                setCharAt map' '@' robo'
                step nextMoves robo'
                // If we are directly blocked, then do not move
            | '#' ->
                // printfn $"#### Blocked! Stay at  {robo.x},{robo.y}"
                step nextMoves robo
            | _ ->
                printfn $"#### Box! Collided with {charAt robo' map'}"
                let affectedBoxes = fillRegion map' robo' dir
                // printfn $"affected boxes %A{affectedBoxes |> List.sortBy _.x |> List.sortBy _.y}"
                // TODO: decide whether to move things
                if (List.isEmpty affectedBoxes) |> not then
                    shiftChars map' 1 '.' dir affectedBoxes
                    setCharAt map' '.' robo
                    setCharAt map' '@' robo'
                    step nextMoves robo'
                else
                    step nextMoves robo

    step moves (getRobot map)
    map'

// let warehouse' = simulate warehouse moves
// printCharMap warehouse 0
// printCharMap warehouse' 0

let sumGPSCoord (map: char array2d) identifier=
    let allBoxes = findChars identifier map
    allBoxes |> List.map (fun (x, y) -> 100 * x + y) |> List.sum
// sumGPSCoord warehouse'

// printCharMap warehouse2 0
// fillRegion warehouse2 (move Le 1 {x=1;y=8}) Le
// fillRegion warehouse2 (move Le 1 robotInit2) Le
// printCharMap (simulate warehouse2 (List.take 2 moves)) 0
//
let warehouse2' = simulate warehouse2 moves ()
sumGPSCoord warehouse2 '['
OK Something wierd is happening and I cannot track it down.
Figure out what is happening

printCharMap warehouse2 0
printCharMap warehouse2' 0
simulate warehouse2 (List.take 20 moves)
printCharMap (simulate warehouse2 (List.take 1 moves)) 0

let testmap =
    [| "##############"
       "##......##..##"
       "##..........##"
       "##....[][]@.##"
       "##....[]....##"
       "##..........##"
       "##############" |]
    |> array2D
let testmoves = "<vv<<^^<<^^" |> Array.ofSeq |> Array.map arrowToDir |> Array.toList
printCharMap testmap 0
getRobot testmap
printCharMap (simulate testmap (List.take 1 testmoves)) 0
printCharMap (simulate testmap (List.take 2 testmoves)) 0
printCharMap (simulate testmap (List.take 3 testmoves)) 0
printCharMap (simulate testmap (List.take 4 testmoves)) 0
printCharMap (simulate testmap (List.take 5 testmoves)) 0
printCharMap (simulate testmap (List.take 6 testmoves)) 0
printCharMap (simulate testmap (List.take 7 testmoves)) 0
printCharMap (simulate testmap (List.take 8 testmoves)) 0
printCharMap (simulate testmap (List.take 9 testmoves)) 0
printCharMap (simulate testmap (List.take 10 testmoves)) 0
printCharMap (simulate testmap (List.take 11 testmoves)) 0
printCharMap (simulate testmap testmoves) 0

fillRegion testmap (move Le 1 (getRobot testmap)) Le     

// the 2nd Up move (6th) breaks. lets examine
let test6th =
    [| "##############"
       "##......##..##"
       "##...[][]...##"
       "##....[]....##"
       "##.....@....##"
       "##..........##"
       "##############" |]
    |> array2D

printCharMap test6th 0
fillRegion test6th (move Up 1 (getRobot test6th)) Up 
// 6th is fixed!

// now onto 11th move. it overrides a box tile with the robot
let test11th =
    [| "##############"
       "##......##..##"
       "##...[][]...##"
       "##...@[]....##"
       "##..........##"
       "##..........##"
       "##############" |]
    |> array2D
printCharMap test11th 0
fillRegion test11th (move Up 1 (getRobot test11th)) Up 
printCharMap (simulate test11th [Up]) 0

let testmap3 =
    [| "##############"
       "##......##..##"
       "##...[][]...##"
       "##....[]....##"
       "##....@[][].##"
       "##..........##"
       "##############" |]
    |> array2D
let teststart = findChars '@' testmap |> List.head |> Point.ofTuple


open SkiaSharp

// Map a character to an SKColor
let charToColor (ch: char) =
    match ch with
    | '[' -> SKColors.Gray
    | ']' -> SKColors.Gray
    | '.' -> SKColors.White
    | '@' -> SKColors.Green
    | '#' -> SKColors.Blue
    | _ -> SKColors.Red

// Save a 2D char array as an image with characters
let saveCharArrayAsImage (charArray: char[,]) (direction: string) (filePath: string) (fontSize: float32) =
    let rows = charArray.GetLength(0)
    let cols = charArray.GetLength(1)

    // Create a bitmap canvas
    let textHeight = fontSize * 2.0f
    let imageWidth = int (cols * int fontSize)
    let imageHeight = int (rows * int fontSize + int textHeight)
    let imageInfo = SKImageInfo(imageWidth, imageHeight)
    use surface = SKSurface.Create(imageInfo)
    let canvas = surface.Canvas

    // Clear the canvas with a default background color
    canvas.Clear(SKColors.DarkGray)

    // Set up paint for text rendering
    use typeface = SKTypeface.FromFamilyName("Monospace", SKFontStyle.Normal)
    use paint = new SKPaint()
    paint.IsAntialias <- true
    paint.TextSize <- fontSize
    paint.Typeface <- SKTypeface.Default

    // Draw the direction at the top of the image
    paint.Color <- SKColors.Black
    let textPosX = float32 (imageWidth / 2) - (paint.MeasureText(direction) / 2.0f)
    let textPosY = fontSize // Position the text
    canvas.DrawText(direction, textPosX, textPosY, paint)
    
    // Draw each character on the canvas
    for y in 0 .. rows - 1 do
        for x in 0 .. cols - 1 do
            let ch = charArray.[y, x]
            let color = charToColor ch
            paint.Color <- color
            let posX = float32 (x * int fontSize)
            let posY = float32 (y * int fontSize) + fontSize + textHeight
            canvas.DrawText(ch.ToString(), posX, posY, paint)

    // Save the image to file
    use image = surface.Snapshot()
    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    use fileStream = System.IO.File.OpenWrite(filePath)
    data.SaveTo(fileStream)

// Save a series of 2D char arrays as images
let saveSeriesOfCharArrays (charArrays: char[,][] ) (outputDir: string) fontSize =
    System.IO.Directory.CreateDirectory(outputDir) |> ignore
    for i = 0 to charArrays.Length - 1 do
        let filePath = System.IO.Path.Combine(outputDir, sprintf "frame_%03d.png" i)
        let dirLabel = $"{moves[i]} (next move is {moves[i + 1]})"
        saveCharArrayAsImage charArrays.[i] dirLabel filePath fontSize

let allFrames =
    [ for i in 1 .. (List.length moves) ->
        simulate warehouse2 (List.take i moves) ] |> Array.ofList
// let charArrays = [| test6th; test11th; testmap3 |]
saveSeriesOfCharArrays allFrames "./output" 42.0f

"asds" + "23423"

// Save the series of 2D char arrays as images
saveSeriesOfCharArrays charArrays "./output"

// Make sure the "./output" directory exists beforehand.