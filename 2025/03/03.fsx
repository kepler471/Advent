#r "nuget: SixLabors.ImageSharp"
#r "nuget: SixLabors.ImageSharp.Drawing"
#load "../utils.fs"
#load "../animate.fs"

open _2025.utils
open _2025.animate
open SixLabors.ImageSharp.PixelFormats

let recorderConfig = {
    scale = 14
    colorMap = fun _ -> Rgba32(180uy, 180uy, 180uy)
    trailColors = []
    backgroundColor = Rgba32(30uy, 30uy, 30uy)
    renderMode = Pixel
}

let banks = Input.lines "03/input" |> Seq.map (Seq.map digitToInt)

type recorderStates = {
    skipWindow: int seq
    // largestBatteries: int seq
    selectedBattery: int
    searchCutoff: int seq
}

// Look from the right of the bank to the left
// Make the function flexible for the number of batteries
// Reverse the bank
// For n batteries, look for b_1 by skipping n - 1 batteries
// ... and so on
let maxJoltage (digits: int) (states: ResizeArray<recorderStates>) (bank: int seq) =
    let lenBank = Seq.length bank
    let rec findMaxNum (search: int seq) (stack: int list) (n: int) =
        let lenSearch = Seq.length search
        let offset = lenBank - lenSearch
        if n = 0 then
            stack |> List.rev |> List.map int64 |> digitsToInt64 
        else
            let largestBattery = search |> Seq.rev |> Seq.skip (n - 1) |> Seq.max
            let batteryLocation = search |> Seq.findIndex ((=) largestBattery)
            
            states.Add({
                skipWindow = [ lenBank - (n - 1) .. lenBank - 1 ]
                // largestBatteries = [ for i in search ]
                selectedBattery = batteryLocation + offset
                searchCutoff = [ 0 .. batteryLocation + offset - 1 ]
            })
            
            findMaxNum (Seq.skip (batteryLocation + 1) search) (largestBattery :: stack) (n - 1)

    findMaxNum bank [] digits

// Collect states per row
let statesByRow =
    banks
    |> Seq.mapi (fun rowIndex bank ->
        let store = ResizeArray<recorderStates>()
        let result = maxJoltage 2 store bank
        rowIndex, result, store |> Seq.toList
    )
    |> Seq.toList

let totalSum = statesByRow |> List.sumBy (fun (_, result, _) -> result)

// Build the grid for visualization
let banksArray = banks |> Seq.map Seq.toArray |> Seq.toArray
let h, w = banksArray.Length, banksArray[0].Length
let grid = Array2D.init h w (fun r c -> char (banksArray[r][c] + int '0'))

// Define colors
let skipColor = Rgba32(100uy, 100uy, 150uy)       // dim blue: skipped window
let selectedColor = Rgba32(0uy, 255uy, 0uy)       // green: selected battery
let cutoffColor = Rgba32(80uy, 80uy, 80uy)        // dim: won't search again

// Create frames
let recorder = FrameRecorder(recorderConfig)

for rowIndex, _, states in statesByRow do
    for state in states do
        // Frame 1: show skip window
        let skipHighlights = state.skipWindow |> Seq.map (fun col -> !@(rowIndex, col), skipColor)
        recorder.AddFrameWithColors(grid, skipHighlights, "Skip window")

        // Frame 2: show selected battery
        recorder.AddFrameWithColors(grid, [ !@(rowIndex, state.selectedBattery), selectedColor ], "Selected")

        // Frame 3: show cutoff region
        let cutoffHighlights = state.searchCutoff |> Seq.map (fun col -> !@(rowIndex, col), cutoffColor)
        recorder.AddFrameWithColors(grid, cutoffHighlights, "Cutoff")

recorder.Save("03/selection.gif", 20)