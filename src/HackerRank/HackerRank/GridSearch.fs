[<AutoOpen>]
module GridSearch

let allIndexOf (str:string) (search:string) =
        let rec inner (s:string) l offset =
            match (s.IndexOf(search), (s.IndexOf(search)+1) = s.Length) with
            | (-1, _) -> l
            | (x, true) -> (x+offset)::l
            | (x, false) -> inner(s.Substring(x+1)) ((x+offset)::l) (x+1)
        inner str [] 0 |> List.rev

let find grid str start = 
    grid  |> Seq.skip start
          |> Seq.mapi (fun i x -> (allIndexOf x str) |> Seq.map (fun u -> (u, i+start))) 
          |> Seq.collect (fun x -> x)
                      
let findExact grid str x y = 
    find grid str y |> Seq.exists(fun (a, b) -> x = a && y = b)
                            
let gridContains grid (search: string list) = 
    let matches = find grid search.[0] 0 
    let numStrs = search |> Seq.skip 1
                         |> Seq.mapi(fun i line -> (i, line))
    matches |> Seq.map(fun (a, b) -> numStrs |> Seq.map(fun (i, line) -> findExact grid line a (b+i+1)))
            |> Seq.exists(fun b -> b |> Seq.forall(fun x -> x))