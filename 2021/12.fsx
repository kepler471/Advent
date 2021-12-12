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

type Connection<'T> = 
    string (* id *) * string (* id *)  * 'T

type Adjacency<'TStruct> = 
    Connection<'TStruct> list

type Atom<'TNode, 'TConnection> = 
    Node<'TNode> * Adjacency<'TConnection>

type Graph<'TNode, 'TConnection> = 
    Atom<'TNode, 'TConnection> list

let GetNodeId (node: Node<'T>) : string = 
    node |> fst
    
let GetAtomId (atom: Atom<_, _>) : string =
    atom |> fst |> fst

let GetAtom (id: string) (graph: Graph<_, _>) : Atom<_, _> option =
    graph
    |> List.tryFind (fun atom -> (GetAtomId atom) = id)

let GetNode (id: string) (graph: Graph<_, _>) : Node<_> option =
    match (GetAtom id graph) with
    | Some (a) -> Some(fst a)
    | None -> None

let AddNode (node: Node<'T>) (graph: Graph<'T,_>): Graph<'T,_> = 
    let id = GetNodeId node
    match (GetNode id graph) with
    | None ->  
        let newAdj : Adjacency<'T> = []
        let newAtom: Atom<'T,_> = (node, newAdj)
        graph @ [newAtom]
    | _ -> graph

let parse (pairs: (string * string) seq) = 
    let uniqueIds (pairs: (string * string) seq) = 
        pairs
        |> Seq.fold (fun acc x -> [fst x; snd x] @ acc) []
        |> Set
    
    let BuildAdjacency (node: int Node) (pairs: (string * string) seq) = 
        let connContains (id: 'a) (conn: 'a * 'a) = 
            fst conn = id || snd conn = id
        
        let adj: int Node Adjacency = []
        pairs
        |> Seq.filter (connContains (GetNodeId node))
        |> Seq.map (fun (a, b) -> Connection(a, b, node))
        |> Seq.fold (fun acc x -> x :: acc) adj

    let BuildGraph (pairs: (string * string) seq) =
        let uids = pairs |> uniqueIds
        let g: Graph<_,_> = []
        uids
        |> Seq.map (fun uid -> 
            let n = Node(uid, 0) 
            n, BuildAdjacency n pairs)
        |> Seq.fold (fun acc x -> x :: acc) g

    BuildGraph pairs

let incVisit (id: string) (graph: Graph<_,_>) =
    ()

let testg = parse input
let testa = GetAtom "kj" testg
testa.Value |> (fst >> snd >> (+) 1)


Node("abc", 0)
GetNode "LN" testg |> (fun x -> match x with 
                                | Some x -> Some (fst x)
                                | None -> None)
let ggg: Atom<int,_> = (Node("HN", 0), BuildAdjacency (Node("HN", 0)) input)
ggg |> snd |> List.head

ggg


// let AddAdjacency (node: )


let testnode:Node<int> = Node("test", 2)
let g: Graph<int,int> = []
let h = AddNode testnode g
