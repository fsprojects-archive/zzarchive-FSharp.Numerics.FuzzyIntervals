namespace FSharp.Fuzzy

open System

[<AutoOpen>]
module FuzzyUtils =
    let alpha level = 0.1*double level

type Fuzzy(a : Interval seq) = 
    let alphas = a |> Array.ofSeq 
    do assert(alphas.Length = 11)   

    member this.alphaCuts with get() = alphas
    member this.Bottom with get() = alphas.[0]
    member this.Top with get() = alphas.[10]
    member this.Plot = 
        seq { for i in 0..10 -> alphas.[i].a, alpha i 
              for i in 10..-1..0 -> alphas.[i].b, alpha i  } |> Array.ofSeq

    static member interval(a,b,c,d) = 
        assert (a<=b && b<=c && c<=d)
        Fuzzy(seq { for i in 0..10 -> { a = a+(b-a)*0.1*double i; b = c+(d-c)*0.1*double (10-i) } } )
    
    static member number(a,b,c) = Fuzzy.interval(a,b,b,c)
    static member point(a) = Fuzzy.number(a,a,a)
    static member Zero =  Fuzzy.point(0.)
    
    static member operation f (a:Fuzzy) (b:Fuzzy) =  Fuzzy(Seq.map2 f a.alphaCuts b.alphaCuts )
    static member map f (a:Fuzzy) =  Fuzzy(Seq.map f a.alphaCuts )

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
[<AutoOpen>]
module Fuzzy =    
    let distance (a: Fuzzy, b: Fuzzy) = 
        let dividend = 
            Seq.zip a.alphaCuts b.alphaCuts 
            |> Seq.mapi (fun i pair -> alpha i * distance pair ) 
            |> Seq.sum
        Math.Max(0., dividend/5.5 )
