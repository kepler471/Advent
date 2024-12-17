#load "../utils.fs"
#r "nuget: Plotly.NET.Interactive, 5.0.0"
#r "nuget: FSharp.Stats"
open FSharp.Stats
open _2024.utils
open System.Text.RegularExpressions
open Plotly.NET

let moduloWrap value limit = ((value % limit) + limit) % limit

let xlim, ylim= 101, 103
// let xlim, ylim= 11, 7
let xmid, ymid = xlim / 2, ylim / 2

type Robot =
    { p: Point; v: Point }
    member this.step' = this.p + this.v
    member this.step  =
        let newP = { x = moduloWrap (this.step'.x) xlim; y = moduloWrap (this.step'.y) ylim }
        { p = newP; v = this.v }

let parseLine (input: string) =
    let matchResult = Regex.Match(input, @"p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)")
    let p = (int matchResult.Groups.[1].Value, int matchResult.Groups.[2].Value)
    let v = (int matchResult.Groups.[3].Value, int matchResult.Groups.[4].Value)
    { p = Point.ofTuple(p); v = Point.ofTuple(v) }

let moveAllRobots (robots: Robot seq) = robots |> Seq.map _.step

let quadrants (map: int array2d) xmid ymid =
    [ map[.. ymid - 1, .. xmid - 1]
      map[ymid + 1 .., .. xmid - 1]
      map[.. ymid - 1, xmid + 1 ..]
      map[ymid + 1 .., xmid + 1 ..] ] |> List.map flattenArray2D

let calcSafetyFactor (map: int array2d) =
    quadrants map xmid ymid |> List.map Seq.sum |> List.reduce (*)

// let calcAverageNonZero (map: int array2d) =
//     map |> flattenArray2D |> Seq.filter (fun x -> x <> 0) |> Seq.averageBy float

// let calcSafetyFactor (robots: Robot seq) =
//     robots |> Seq.co  DO THIS FOR THE Robot Seq TYPE RATHER THAN ON THE ARRAY untBy _.p |> Seq.map snd |> Seq.averageBy float
     
let calcAverageNonZero (robots: Robot seq) =
    robots |> Seq.countBy _.p |> Seq.map snd |> Seq.averageBy float

let calcVarX (robots: Robot seq) =
    robots |> varBy (_.p >> _.x >> float)
let calcVarY (robots: Robot seq) =
    robots |> varBy (_.p >> _.y >> float)

let generateEBHQ robots =
    let EBHQ = Array2D.init ylim xlim (fun _ _ -> 0)
    robots |> Seq.iter (fun robot -> EBHQ[robot.p.y, robot.p.x] <- EBHQ[robot.p.y, robot.p.x] + 1)
    EBHQ

let robots = System.IO.File.ReadAllLines("2024/14/input") |> Seq.map parseLine
let finalState = iterN 100 moveAllRobots robots
let EBHQ = generateEBHQ finalState
printIntMap EBHQ 0
calcSafetyFactor EBHQ

let searchForTree robots maxTime fn =
    let rec loop robots step acc =
        if step >= maxTime then
            acc, robots
        else
            let robots' = moveAllRobots robots
            // let EBHQ = generateEBHQ robots'
            let value = fn robots'
            // let value = fn EBHQ
            loop robots' (step + 1) (value :: acc)
    loop robots 0 []
    
// let safs = searchForTree robots 10_000 calcSafetyFactor
let varXs, robots' = searchForTree robots 10_000 calcVarX

let avgs, robots' = searchForTree robots 2_000 calcAverageNonZero
let avgs1, robots' = searchForTree robots' 2_000 calcAverageNonZero
let avgs2, robots' = searchForTree robots' 2_000 calcAverageNonZero
let avgs3, robots' = searchForTree robots' 2_000 calcAverageNonZero
let avgs4, robots' = searchForTree robots' 2_000 calcAverageNonZero
let allavgs = [avgs; avgs1; avgs2; avgs3; avgs4] |> List.collect id

let safs = searchForTree (iterN 7100 moveAllRobots robots) 300 calcSafetyFactor

// robots
// |> List.fold

// safs |> List.findIndex (fun x -> x = (safs |> List.reduce max))
// safs |> List.reduce max
// safs |> List.findIndex (fun x -> x = (safs |> List.reduce min))
// safs |> List.reduce min
avgs |> List.findIndex (fun x -> x = (avgs |> List.reduce max))
avgs |> List.reduce max
avgs |> List.findIndex (fun x -> x = (avgs |> List.reduce min))
avgs |> List.reduce min
82, 305, 671, 570
let xvals = allavgs |> List.mapi (fun index _ -> index)
let xvals = fst safs |> List.mapi (fun index _ -> index)
(iterN 7338 moveAllRobots robots |> generateEBHQ, 0) ||> printIntMap


robots |> varBy (_.p >> _.x >> float)
iterN 7338 moveAllRobots robots |> Seq.varBy (_.p >> _.x >> float)

iterN 62 moveAllRobots robots |> generateEBHQ |> calcSafetyFactor

// avgs |> List.map calcAverageNonZero |> List.fold max 0
// avgs |> List.map calcSafetyFactor |> List.fold max 0

let chart1 = Chart.Line(xvals, fst safs, Name = "Safety Factor")
let chart2 = Chart.Line(xvals, allavgs, Name = "Average Non-Zero")
chart1 |> Chart.show
[ chart1; chart1 ] |> Chart.combine |> Chart.show
[ chart1; chart2 ] |> Chart.Grid(2, 1) |> Chart.show