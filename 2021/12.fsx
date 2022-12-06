(*
--- Day 12: Passage Pathing ---

With your submarine's subterranean subsystems subsisting suboptimally, the only way you're getting out of this cave anytime soon is by finding a path yourself. Not just a path - the only way to know if you've found the best path is to find all of them.

Fortunately, the sensors are still mostly working, and so you build a rough map of the remaining caves (your puzzle input). For example:

start-A
start-b
A-c
A-b
b-d
A-end
b-end

This is a list of how all of the caves are connected. You start in the cave named start, and your destination is the cave named end. An entry like b-d means that cave b is connected to cave d - that is, you can move between them.

So, the above cave system looks roughly like this:

    start
    /   \
c--A-----b--d
    \   /
     end

Your goal is to find the number of distinct paths that start at start, end at end, and don't visit small caves more than once. There are two types of caves: big caves (written in uppercase, like A) and small caves (written in lowercase, like b). It would be a waste of time to visit any small cave more than once, but big caves are large enough that it might be worth visiting them multiple times. So, all paths you find should visit small caves at most once, and can visit big caves any number of times.

Given these rules, there are 10 paths through this example cave system:

start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,end
start,A,c,A,b,A,end
start,A,c,A,b,end
start,A,c,A,end
start,A,end
start,b,A,c,A,end
start,b,A,end
start,b,end

(Each line in the above list corresponds to a single path; the caves visited by that path are listed in the order they are visited and separated by commas.)

Note that in this cave system, cave d is never visited by any path: to do so, cave b would need to be visited twice (once on the way to cave d and a second time when returning from cave d), and since cave b is small, this is not allowed.

Here is a slightly larger example:

dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc

The 19 paths through it are as follows:

start,HN,dc,HN,end
start,HN,dc,HN,kj,HN,end
start,HN,dc,end
start,HN,dc,kj,HN,end
start,HN,end
start,HN,kj,HN,dc,HN,end
start,HN,kj,HN,dc,end
start,HN,kj,HN,end
start,HN,kj,dc,HN,end
start,HN,kj,dc,end
start,dc,HN,end
start,dc,HN,kj,HN,end
start,dc,end
start,dc,kj,HN,end
start,kj,HN,dc,HN,end
start,kj,HN,dc,end
start,kj,HN,end
start,kj,dc,HN,end
start,kj,dc,end

Finally, this even larger example has 226 paths through it:

fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW

How many paths through this cave system are there that visit small caves at most once?

Your puzzle answer was 3856.
--- Part Two ---

After reviewing the available paths, you realize you might have time to visit a single small cave twice. Specifically, big caves can be visited any number of times, a single small cave can be visited at most twice, and the remaining small caves can be visited at most once. However, the caves named start and end can only be visited exactly once each: once you leave the start cave, you may not return to it, and once you reach the end cave, the path must end immediately.

Now, the 36 possible paths through the first example above are:

start,A,b,A,b,A,c,A,end
start,A,b,A,b,A,end
start,A,b,A,b,end
start,A,b,A,c,A,b,A,end
start,A,b,A,c,A,b,end
start,A,b,A,c,A,c,A,end
start,A,b,A,c,A,end
start,A,b,A,end
start,A,b,d,b,A,c,A,end
start,A,b,d,b,A,end
start,A,b,d,b,end
start,A,b,end
start,A,c,A,b,A,b,A,end
start,A,c,A,b,A,b,end
start,A,c,A,b,A,c,A,end
start,A,c,A,b,A,end
start,A,c,A,b,d,b,A,end
start,A,c,A,b,d,b,end
start,A,c,A,b,end
start,A,c,A,c,A,b,A,end
start,A,c,A,c,A,b,end
start,A,c,A,c,A,end
start,A,c,A,end
start,A,end
start,b,A,b,A,c,A,end
start,b,A,b,A,end
start,b,A,b,end
start,b,A,c,A,b,A,end
start,b,A,c,A,b,end
start,b,A,c,A,c,A,end
start,b,A,c,A,end
start,b,A,end
start,b,d,b,A,c,A,end
start,b,d,b,A,end
start,b,d,b,end
start,b,end

The slightly larger example above now has 103 paths through it, and the even larger example now has 3509 paths through it.

Given these new rules, how many paths through this cave system are there?

Your puzzle answer was 116692.

Both parts of this puzzle are complete! They provide two gold stars: **
*)

type Size =
    | Big
    | Small

// TODO: How would I make id generic type
type Node<'T> =
    string (* id *)  * 'T (* node data *)

type Connection =
    string (* id *) * string (* id *)

type Adjacency =
    Connection list

type Atom<'TNode> =
    Node<'TNode> * Adjacency

type Graph<'TNode> =
    Atom<'TNode> list

// TODO: Just use a Map instead of these Get functions?
//      Would also have to change the Graph to be Map
let GetNodeId (node: 'T Node) : string =
    node |> fst

let GetAtomId (atom: 'T Atom) : string =
    atom |> fst |> fst

let GetAtomAdj (atom: 'T Atom) : Adjacency =
    atom |> snd

let GetAtom (id: string) (graph: 'T Graph) : 'T Atom option =
    graph
    |> List.tryFind (fun atom -> (GetAtomId atom) = id)

let GetNode (id: string) (graph: 'T Graph) : 'T Node option =
    match (GetAtom id graph) with
    | Some (a) -> Some(fst a)
    | None -> None

let AddNode (node: Node<'T>) (graph: 'T Graph): 'T Graph =
    let id = GetNodeId node
    match (GetNode id graph) with
    | None ->
        let newAdj : Adjacency = []
        let newAtom: 'T Atom = (node, newAdj)
        graph @ [newAtom]
    | _ -> graph

let uniqueIds (pairs: ('a * 'a) seq) =
    pairs
    |> Seq.fold (fun acc x -> [fst x; snd x] @ acc) []
    |> Set

let parse (init: 'T) (pairs: (string * string) seq) =
    let BuildAdjacency (node: 'T Node) (pairs: (string * string) seq) =
        let connContains (id: 'a) (conn: 'a * 'a) =
            fst conn = id || snd conn = id

        let adj: Adjacency = []
        pairs
        |> Seq.filter (connContains (GetNodeId node))
        |> Seq.map (fun (a, b) -> Connection(a, b))
        |> Seq.fold (fun acc x -> x :: acc) adj

    let BuildGraph (init: 'T) (pairs: (string * string) seq) =
        let uids = pairs |> uniqueIds
        let g: 'T Graph = []
        uids
        |> Seq.map (fun uid ->
            let n = Node(uid, init)
            Atom(n, BuildAdjacency n pairs))
        |> Seq.fold (fun acc x -> x :: acc) g

    BuildGraph init pairs

let pathfinder (start: string) (finish: string) (mutate: 'T -> 'T) (rule: 'T Node -> bool) (graph: 'T Graph) =
    let onVisit (incrementer: 'T -> 'T) (node: 'T Node) =
        Node(fst node, snd node |> incrementer)

    let getSize (node: 'T Node) =
        if fst node = (fst node).ToUpper() then Big else Small

    let rec pathfind (a: 'T Atom) (b: 'T Atom) (graph: 'T Graph) (path: string list) =
        let id = GetAtomId a

        match id with
        | id when id = GetAtomId b -> [id :: path]
        | _ ->
            let newG =
                graph
                |> List.map (fun (n, adj) ->
                    if GetNodeId n = id then (onVisit mutate n, adj) else (n, adj))

            let choices = (GetAtomAdj a |> uniqueIds) - Set([GetAtomId a])

            choices
            |> Seq.filter (fun c ->
                let node = (GetNode c newG).Value
                rule node)
            |> Seq.fold (fun acc c ->
                pathfind (GetAtom c graph).Value b newG (id :: path) @ acc) []

    pathfind (GetAtom start graph).Value (GetAtom finish graph).Value graph []

// Solve
let input =
    System.IO.File.ReadAllLines "2021/inputs/12.txt"
    |> Array.map (fun x -> x.Split "-")
    |> Array.map (fun x -> x.[0], x.[1])

let finder = pathfinder "start" "end" ((+)1)

// Visit Small caves only once
let rule1 = (fun x ->
    GetNodeId x = (GetNodeId x).ToUpper() || x |> snd < 1)

// Visit one Small cave up to twice, and all other Small caves only once
let rule2 = (fun i x->
    GetNodeId x = (GetNodeId x).ToUpper() || i = GetNodeId x && x |> snd < 2 || x |> snd < 1)

let mutatedInputs =
    input
    |> uniqueIds
    |> Seq.filter (fun x -> x <> "start" && x <> "end" && x.ToLower() = x)

printfn "%A" "--- Day 12: Passage Pathing ---"
printfn "Part One: %A" (parse 0 input |> finder rule1 |> Set |> Seq.length)
printfn "Part Two: %A" (mutatedInputs |> Seq.map (fun i -> parse 0 input |> finder (rule2 i)) |> Seq.concat |> Set |> Seq.length)
