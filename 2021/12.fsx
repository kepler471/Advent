// type Cave =
//     | Id of string
//     | Connections of Cave list
//     | Size of Size

let input = 
    [|
        "dc-end"
        "HN-start"
        "start-kj"
        "dc-start"
        "dc-HN"
        "LN-dc"
        "HN-end"
        "kj-sa"
        "kj-HN"
        "kj-dc"
    |]
    |> Array.map (fun x -> x.Split "-")
    |> Array.map (fun x -> x.[0], x.[1])

type Size =
    | Big
    | Small

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

let parse (pairs: (string * string) seq) = 
    let uniqueIds (pairs: (string * string) seq) = 
        pairs
        |> Seq.fold (fun acc x -> [fst x; snd x] @ acc) []
        |> Set
    
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
            n, BuildAdjacency n pairs)
        |> Seq.fold (fun acc x -> x :: acc) g

    BuildGraph 0 pairs

let incVisit (incrementer: 'T -> 'T) (node: 'T Node) =
    Node(fst node, snd node |> incrementer)

let getSize (node: 'T Node) = 
    if fst node = (fst node).ToUpper() then Big else Small

let pathfinder (start: 'T Node) (finish: 'T Node) (graph: 'T Graph) = 
    ()


/// Testing zone
let testg = parse input
let testa = GetAtom "kj" testg
let testn = GetNode "kj" testg
testa.Value |> (fst >> snd >> (+) 1)
testn.Value |> fst
Node("abc", 0) |> incVisit ((+) 1)
GetNode "LN" testg |> (fun x -> match x with 
                                | Some x -> Some (fst x)
                                | None -> None)

let testnode:Node<int> = Node("test", 2)
let g: int Graph = []
let h = AddNode testnode g
