module GridSearchTests

open NUnit.Framework
open FsUnit
open GridSearch

module allIndexOf =
    [<Test>]
    let ``the string 'hello hello' contains the string 'hell' at indexes 0 and 6``() =
        allIndexOf "hello hello" "hell" |> should equal [0; 6]

    [<Test>]
    let ``the string 'hello world' does not contain the string 'kevin' ``() =
        allIndexOf "hello world" "kevin" |> should equal []

module find =
    [<Test>]
    let ``the string '123' is in the grid '444 123 444' at [(0,1)]``() =
        find ["444"; "123"; "444"] "123" 0 |> should equal [(0,1)]

    [<Test>]
    let ``the string '123' is in the grid '444123 981231 123122' at [(0,1)]``() =
        find ["444123"; "981231"; "123122"] "123" 0 |> should equal [(3,0); (2,1); (0,2)]

    [<Test>]
    let ``the string '123' is not in the grid '123 777 444' when the first row is skipped ``() =
        find ["123"; "777"; "444"] "123" 1 |> should equal []

module findExact =
    [<Test>]
    let ``the string '123' is in the grid '444 123 444' is exactly at 0, 1``() =
        findExact ["444"; "123"; "444"] "123" 0 1 |> should equal true

    [<Test>]
    let ``the string '77' is in the grid '9999 9977 0987 0987' is exactly at 2, 1``() =
        findExact ["9999"; "9977"; "0987"; "0987"] "77" 2 1 |> should equal true

    [<Test>]
    let ``the string '865' is not in the grid '9999 9977 0987 0987' is exactly at 2, 1``() =
        findExact ["9999"; "9977"; "0987"; "0987"] "865" 2 1 |> should equal false

    [<Test>]
    let ``the string '77' is not in the grid '9999 9977 0987 0987' is exactly at 3, 3``() =
        findExact ["9999"; "9977"; "0987"; "0987"] "77" 3 3 |> should equal false

module gridContains =
    [<Test>]
    let ``the grid '444 123 444 555' contains the grid '123 444 555 ``() =
        gridContains  ["444"; "123"; "444"; "555"] ["444"; "123"; "444"] |> should equal true

    [<Test>]
    let ``the grid '44488 12311 44490 55598 778899' contains the grid '44 55 ``() =
        gridContains  ["44488"; "12311"; "44490"; "55598"; "778899";] ["44"; "55"] |> should equal true

    [<Test>]
    let ``the grid '44488 12311 44490 55598 778899' does not contain the grid '88 88 ``() =
        gridContains  ["44488"; "12311"; "44490"; "55598"; "778899";] ["88"; "88"] |> should equal false