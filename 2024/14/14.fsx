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
            acc
        else
            let robots' = moveAllRobots robots
            let value = fn robots'
            loop robots' (step + 1) (value :: acc)
    loop robots 0 []
    
let r7000 = (iterN 7300 moveAllRobots robots)
let varXs= searchForTree r7000 100 calcVarX
let varYs= searchForTree r7000 100 calcVarX

// safs |> List.findIndex (fun x -> x = (safs |> List.reduce max))
let xvals = varXs |> List.mapi (fun index _ -> index)
(iterN 7338 moveAllRobots robots |> generateEBHQ, 0) ||> printIntMap
iterN 7338 moveAllRobots robots |> varBy (_.p >> _.x >> float)

let chart1 = Chart.Line(xvals, varXs, Name = "X-variance")
let chart2 = Chart.Line(xvals, varYs, Name = "Y-variance")
[ chart1; chart2 ] |> Chart.Grid(2, 1) |> Chart.show