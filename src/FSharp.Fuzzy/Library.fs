namespace FSharp.Fuzzy

open System

type Fuzzy(a : Interval array) = 
    do assert(a.Length = 10)   

    member this.alphaCuts with get() = Array.AsReadOnly a
    
    static member interval(a,b,c,d) = 
        assert (a<=b && b<=c && c<=d)
        Fuzzy(seq { for i in 0..9 -> { a = a+(b-a)*0.1*double i; b = c+(d-c)*0.1*double i } } |> Array.ofSeq )
    
    static member number(a,b,c) = Fuzzy.interval(a,b,b,c)
    
    static member operation f (a:Fuzzy) (b:Fuzzy) =  Fuzzy(Seq.map2 f a.alphaCuts b.alphaCuts |> Array.ofSeq)
    static member map f (a:Fuzzy) =  Fuzzy(Seq.map f a.alphaCuts |> Array.ofSeq)

    static member (*) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a*b) x y
    static member (/) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a/b) x y
    static member (+) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a+b) x y
    static member (-) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a-b) x y
    static member (*) (x : Fuzzy, y : double) = Fuzzy.map (fun a-> a*y) x
    static member (/) (x : Fuzzy, y : double) = Fuzzy.map (fun a-> a/y) x
    static member (+) (x : Fuzzy, y : double) = Fuzzy.map (fun a-> a+y) x
    static member (-) (x : Fuzzy, y : double) = Fuzzy.map (fun a-> a-y) x
    static member (*) (x : double, y : Fuzzy) = y * x
    static member (/) (x : double, y : Fuzzy) = Fuzzy.map (fun a-> x/a) y
    static member (+) (x : double, y : Fuzzy) = y + x
    static member (-) (x : double, y : Fuzzy) = Fuzzy.map (fun a-> x/a) y
    static member pow (x : Fuzzy, p : double) = Fuzzy.map (fun a-> Interval.pow(a, p)) x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Fuzzy =    
    let fuzziness = ()
