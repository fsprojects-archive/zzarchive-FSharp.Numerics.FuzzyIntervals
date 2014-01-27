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
let factory() =
    let number = trapezoid 5 (1m,2m,2m,3m)
    let sut = number.alphaCuts
    sut.[0] |> should equal { a = 1m; b=3m }
    sut.[2] |> should equal { a = 1.5m; b=2.5m }
    sut.[4] |> should equal { a = 2m; b=2m }

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

[<Test>] 
let ``operations for non-standard number of alhpa-cuts``() =
    let fiveLevels (a,b,c) = trapezoid 5 (a,b,b,c)
    let num = fiveLevels (1m,2m,3m)
    num + num |> should equal (fiveLevels(2m,4m,6m))
    num * 10m |> should equal (fiveLevels(10m,20m,30m))
    
let testScew (uniform : Fuzzy) (scewed : Fuzzy) = 
    uniform.Top |> should equal scewed.Top
    uniform.Bottom |> should equal scewed.Bottom
    let middle = uniform.alphaCuts.Length / 2
    uniform.alphaCuts.[middle] |>  should not' (equal scewed.alphaCuts.[middle])

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

[<Test>] 
let width() =
    let num = 1m,2m,2m,3m
    width (trapezoid 11 num) |> should equal 0.6m
    width (trapezoid 2 num) |> should equal 0m
    width (trapezoid 5 num) |> should equal 0.5m
    