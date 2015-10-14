module ManasaAndStonesTests

open NUnit.Framework
open FsUnit
open ManasaAndStones

module power =
    [<Test>]
    let ``2^3 should be 8``() =
        power 2 3 |> should equal 8 

    [<Test>]
    let ``10^3 should be 1000``() =
        power 10 3 |> should equal 1000

    [<Test>]
    let ``9^2 should be 81``() =
        power 9 2 |> should equal 81



module combinations =
    [<Test>]
    let ``combinations for [2;3] of length 2 should be [[2;2];[2;3];[3;2];[3;3]]``() =
        combinations [2;3] 2 |> should equal [[2;2];[2;3];[3;2];[3;3]]

    [<Test>]
    let ``combinations for [2;3] of length 3 should be [[2;2;2];[2;2;3];[2;3;2];[2;3;3];[3;2;2];[3;2;3];[3;3;2];[3;3;3]]``() =
        combinations [2;3] 3 |> should equal [[2;2;2];[2;2;3];[2;3;2];[2;3;3];[3;2;2];[3;2;3];[3;3;2];[3;3;3]]

    [<Test>]
    let ``combinations for [1;4] of length 1 should be [[1];[4]]``() =
        combinations [1;4] 1 |> should equal [[1];[4]]



    
      