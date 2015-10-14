[<AutoOpen>]
module ManasaAndStones

open System

let power x y =
    let rec inner a b =
        match (a, b) with
        | (a, 0) -> a
        | (a, b) -> inner (a*x) (b-1)
    inner x (y-1)

let combinations list length =
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



    
