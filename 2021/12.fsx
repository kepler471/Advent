// type Cave =
//     | Id of string
//     | Connections of Cave list
//     | Size of Size

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

let GetNodeId (node: Node<'T>) : 
    string = node |> fst
    
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
        let newAtom: Atom<'T,_> = (node, [])
        graph @ [newAtom]
    | _ -> graph

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

let connContains (id: string) (conn: string * string) = 
    fst conn = id || snd conn = id

let readConn (id: string) (a: string, b: string) = 
    match a, b with
    | a, b when b = id -> Some(b, a)
    | a, b when a = id -> Some(a, b)
    | _ -> None

let getUniqueNodeIds (pairs: (string * string) []) = 
    pairs
    |> Array.fold (fun acc x -> [fst x; snd x] @ acc) []
    |> Set

let uids = input |> getUniqueNodeIds

let Build (uin: string) (pairs: seq<string * string>) = 
        pairs
        |> Seq.filter (connContains uin)
        |> Seq.map (fun (a, b) -> Connection(a, b, Node(uin, 0)))
        |> Seq.fold (fun acc x -> x :: acc) []

Build "HN" input

let testnode:Node<int> = Node("test", 2)
let g: Graph<int,int> = []
AddNode testnode g
