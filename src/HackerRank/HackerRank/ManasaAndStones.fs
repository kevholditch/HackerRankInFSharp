[<AutoOpen>]
module ManasaAndStones

open System
open System.IO

let combinationsold list length =
    let rec solve letters len =     
        seq {
            if len = 1 then 
                for l in letters do yield [l]
            else
                let theRest = solve letters (len-1) 
                for l in letters do
                    for r in theRest do 
                        yield l::r
        }
    solve list length

let combinations list length =
    let rec solve lst len =
        match len with
        | 1 -> lst |> Seq.map(fun x -> [x])
        | i -> lst |> Seq.map(fun l -> solve lst (len-1) |> Seq.map(fun r -> l::r)) 
                            |> Seq.collect(fun x -> x) 
    solve list length

let findMansaNumbers steps (stone1:int) (stone2:int) =
    combinations [stone1; stone2] (steps-1) |> Seq.map(fun l -> Seq.sum(l)) 
                                            |> Seq.distinct 
                                            |> Seq.sort
                                            |> fun x -> String.Join(" ", x)


let findNumbers steps (stone1:int) (stone2:int) =
    let list1 = [0..steps-1]
    let list2 = [0..steps-1] |> List.rev
    List.zip list1 list2 |> Seq.map(fun (x, y) -> (x*stone1)+(y*stone2))
                         |> Seq.distinct
                         |> Seq.sort
                         |> fun x -> String.Join(" ", x)

let slowsolution =
    let reader = new StreamReader("ManasaAndStones.txt")
    let read = reader.ReadLine

    let getNum =  fun _ -> read() |> Convert.ToInt32
    
    [for _ in 1..getNum() do findMansaNumbers (getNum()) (getNum()) (getNum()) |> fun str -> printfn "%s" str]

                                                  
    reader.Close()


let solution = 
    let reader = new StreamReader("ManasaAndStones.txt")
    let read = reader.ReadLine
    let getNum =  fun _ -> read() |> Convert.ToInt32
    
    [for _ in 1..getNum() do findNumbers (getNum()) (getNum()) (getNum()) |> fun str -> printfn "%s" (str.ToString())]

                                                  
    reader.Close()