module FSharp.Fuzzy.Tests.Fuzzy

open FSharp.Fuzzy
open NUnit.Framework
open FsUnit

[<Test>] 
let ``alpha-cuts``() =
    let number = number(10m,20m,30m)
    let sut = number.alphaCuts
    sut.[0] |> should equal number.Bottom
    sut.[10] |> should equal number.Top

[<Test>] 
let Plot() =
    let number = number(10m,20m,30m)
    let sut = plot number
    sut |> should haveLength 22
    [sut.[0]; sut.[11]; sut.[21]] |> should equal [(10m, 0m); (20m, 1m); (30m, 0m)]

[<Test>] 
let constructors() =
    interval (1m,2m,2m,3m) = number(1m,2m,3m) |> should be True
    Fuzzy.Zero = Fuzzy.point 0m |> should be True
    
[<Test>] 
let operations() =
    let num = number(1m,2m,3m)
    num * 10m |> should equal (number(10m,20m,30m))
    num * 10m |> should equal (10m * num)
    number(10m,20m,30m) / 10m |> should equal num
    num + num |> should equal (number(2m,4m,6m))
    num + 1m |> should equal (number(2m,3m,4m))
    1m + num |> should equal (number(2m,3m,4m))
    num - num |> should equal (number(-2m,0m,2m))
    1m - num |> should equal (number(-2m,-1m,0m))
    num - 1m |> should equal (number(0m,1m,2m))

let testScew (uniform : Fuzzy) (scewed : Fuzzy) = 
    uniform.Top |> should equal scewed.Top
    uniform.Bottom |> should equal scewed.Bottom
    uniform.alphaCuts.[5] |>  should not' (equal scewed.alphaCuts.[5])

[<Test>] 
let ``Fuzzy Multiplication and Division scew the plot``() =
    testScew (number(4m,10m,18m)) (number(1m,2m,3m) * number(4m,5m,6m))
    testScew (number(1m,1.5m,3m)) (3m / number(1m,2m,3m))
    testScew (number(0.25m,1m,4m)) (number(2m,4m,8m) / number(2m,4m,8m))
    testScew (number(1m,4m,9m)) (Fuzzy.pow(number(1m,2m,3m), 2.))

[<Test>] 
let distnace() =
    let uniform = number(1m,2m,3m)
    distance uniform (number(4m,5m,6m)) |> should equal 3m
    let scewed = Fuzzy.pow(number(1m,2m,3m), 2.)
    distance scewed (number(1m,4m,9m)) |> should equal 0.15m
