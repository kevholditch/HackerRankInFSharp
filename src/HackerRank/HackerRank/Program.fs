// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Application

open System
open System.IO

[<EntryPoint>]
    let main argv = 

    let reader = new StreamReader("TextFile1.txt")

    let read = reader.ReadLine

    let t = read() |> Convert.ToInt32

    let num = fun() -> read().Split(' ') 
                    |> Seq.head
                    |> Convert.ToInt32

    let fetch = fun() -> [for _ in 1..num() do yield read()]  

    let allIndexOf (str:string) (c:string) =
        let rec inner (s:string) l offset =
            match (s.IndexOf(c), (s.IndexOf(c)+1) = s.Length) with
            | (-1, _) -> l
            | (x, true) -> (x+offset)::l
            | (x, false) -> inner(s.Substring(x+1)) ((x+offset)::l) (x+1)
        inner str [] 0
    

    let find g str start = g  |> Seq.skip start
                              |> Seq.mapi (fun i x -> (allIndexOf x str) |> Seq.map (fun u -> (u, i+start))) 
                              |> Seq.collect (fun x -> x)
                      
    let findExact g str x y = find g str y
                              |> Seq.exists(fun (a, b) -> x = a && y = b)
                            
    let gridContains g (s: string list) = 
                        let matches = find g s.[0] 0 
                        let numStrs = s |> Seq.skip 1
                                        |> Seq.mapi(fun i line -> (i, line))
                        matches |> Seq.map(fun (a, b) -> numStrs |> Seq.map(fun (i, line) -> findExact g line a (b+i+1)))
                                |> Seq.exists(fun b -> b |> Seq.forall(fun x -> x))

       
    let answers = [1..t] |> Seq.map(fun x -> gridContains (fetch()) (fetch())) |> Seq.toList
                     
                     
    let print b = match b with
                    | true -> Console.WriteLine("YES")
                    | false -> Console.WriteLine("NO")                 
                    
    answers |> Seq.map(fun a -> print a) |> Seq.toList
                       
    reader.Close()

    printfn "done"


    Console.ReadLine()
    0 // return an integer exit code
