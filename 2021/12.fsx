// type Cave =
//     | Id of string
//     | Connections of Cave list
//     | Size of Size

let input = 
    System.IO.File.ReadAllLines "2021/inputs/12.txt"
    |> Array.map (fun x -> x.Split "-")
    |> Array.map (fun x -> x.[0], x.[1])

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

let pathfinder (start: string) (finish: string) (inc: 'T -> 'T) (graph: 'T Graph) =
    let incVisit (incrementer: 'T -> 'T) (node: 'T Node) =
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
                    if GetNodeId n = id then (incVisit inc n, adj) else (n, adj))

            let choices = (GetAtomAdj a |> uniqueIds) - Set([GetAtomId a])

            choices
            |> Seq.filter (fun c ->
                let node = (GetNode c newG).Value
                (node |> getSize = Big) || node |> snd < 1) // TODO: Fix to be generic
            |> Seq.fold (fun acc c ->
                pathfind (GetAtom c graph).Value b newG (id :: path) @ acc) []

    pathfind (GetAtom start graph).Value (GetAtom finish graph).Value graph []


let paths = parse 0 input |> pathfinder "start" "end" ((+)1)
[ for i in paths -> printfn "%A" (List.rev i) ]
paths |> List.length
input