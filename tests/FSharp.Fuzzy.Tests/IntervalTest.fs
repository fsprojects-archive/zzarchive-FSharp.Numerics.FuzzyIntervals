module FSharp.Fuzzy.Tests.Interval

open FSharp.Fuzzy
open NUnit.Framework
open FsUnit

[<Test>] 
let Middle () = 
    {a = 1m; b = 23m}.Middle |> should equal 12m
    {a = 10m; b = 10m}.Middle |> should equal 10m
    {a= -10m; b = 10m}.Middle |> should equal 0m
    {a= -10m; b = -2m}.Middle |> should equal -6m

[<Test>] 
let Multiply () = 
    {a = 1m; b = 23m} * { a = 1m; b = 2m } |> should equal { a= 1m; b = 46m }
    {a = -2m; b = 2m} * { a = 2m; b = 3m } |> printf "%A"
    {a = -2m; b = 2m} * { a = 2m; b = 3m } |> should equal { a= -6m; b = 6m }
    2m * {a = -2m; b = 2m} * 2m |> should equal { a= -8m; b = 8m }

[<Test>] 
let Divide () = 
    {a = 1m; b = 23m} / { a = 1m; b = 2m } |> should equal { a= 0.5m; b = 23m }
    {a = -2m; b = 2m} / { a = 2m; b = 3m } |> should equal { a= -1m; b = 1m }
    8m / { a = 2m; b = 4m } / 2m |> should equal { a= 1m; b = 2m }
    (fun ()-> {a = -2m; b = 2m} / { a = -2m; b = 3m } |> ignore) |> should throw typeof<System.Exception> 

[<Test>] 
let summation () = 
    {a = 1m; b = 23m} + { a = 1m; b = 2m } |> should equal { a= 2m; b = 25m }
    {a = -2m; b = 2m} + { a = 2m; b = 3m } |> should equal { a= 0m; b = 5m }
    1m + {a = -2m; b = 2m} + 3m  |> should equal { a= 2m; b = 6m }
    
[<Test>] 
let substraction () = 
    {a = 1m; b = 23m} - { a = 1m; b = 2m } |> should equal { a= -1m; b = 22m }    
    {a = -2m; b = 2m} - { a = 2m; b = 3m } |> should equal { a= -5m; b =0m }
    8m - {a = -2m; b = 2m} - 3m |> should equal { a= 3m; b = 7m }

[<Test>] 
let power () = 
    Interval.pow({a = 1m; b = 2m}, 2.) |> should equal { a= 1m; b = 4m }    

[<Test>] 
let zero () = 
    Interval.Zero |> should equal { a= 0m; b = 0m }    

[<Test>] 
let distance () = 
    Interval.distance ({a = 1m; b = 5m}, { a = 1m; b = 2m }) |> should equal 1.5m
    Interval.distance ({ a = 1m; b = 2m }, {a = 1m; b = 5m}) |> should equal 1.5m
