module FSharp.Fuzzy.Tests

open FSharp.Fuzzy
open NUnit.Framework
open FsUnit

[<Test>] 
let Middle () = 
    {a = 1.; b = 23.}.Middle |> should equal 12.
    {a = 10.; b = 10.}.Middle |> should equal 10.
    {a= -10.; b = 10.}.Middle |> should equal 0.
    {a= -10.; b = -2.}.Middle |> should equal -6.

[<Test>] 
let Multiply () = 
    {a = 1.; b = 23.} * { a = 1.; b = 2. } |> should equal { a= 1.; b = 46. }
    {a = -2.; b = 2.} * { a = 2.; b = 3. } |> printf "%A"
    {a = -2.; b = 2.} * { a = 2.; b = 3. } |> should equal { a= -6.; b = 6. }
    2. * {a = -2.; b = 2.} * 2. |> should equal { a= -8.; b = 8. }

[<Test>] 
let Divide () = 
    {a = 1.; b = 23.} / { a = 1.; b = 2. } |> should equal { a= 0.5; b = 23. }
    {a = -2.; b = 2.} / { a = 2.; b = 3. } |> should equal { a= -1.; b = 1. }
    8. / { a = 2.; b = 4. } / 2. |> should equal { a= 1.; b = 2. }
    (fun ()-> {a = -2.; b = 2.} / { a = -2.; b = 3. } |> ignore) |> should throw typeof<System.Exception> 

[<Test>] 
let summation () = 
    {a = 1.; b = 23.} + { a = 1.; b = 2. } |> should equal { a= 2.; b = 25. }
    {a = -2.; b = 2.} + { a = 2.; b = 3. } |> should equal { a= 0.; b = 5. }
    1. + {a = -2.; b = 2.} + 3.  |> should equal { a= 2.; b = 6. }
    
[<Test>] 
let substraction () = 
    {a = 1.; b = 23.} - { a = 1.; b = 2. } |> should equal { a= -1.; b = 22. }    
    {a = -2.; b = 2.} - { a = 2.; b = 3. } |> should equal { a= -5.; b =0. }
    8. - {a = -2.; b = 2.} - 3. |> should equal { a= 3.; b = 7. }
