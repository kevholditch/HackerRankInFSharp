[<AutoOpen>]
module ManasaAndStones

open System

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
