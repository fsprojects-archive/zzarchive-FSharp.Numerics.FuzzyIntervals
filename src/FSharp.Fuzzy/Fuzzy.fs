namespace FSharp.Fuzzy

open System

[<AutoOpen>]
module FuzzyUtils =
    let alpha level = 0.1m * decimal level

[<StructuredFormatDisplayAttribute("{alphaCuts}")>]
type Fuzzy(a : Interval seq) = 
    let alphas = a |> Array.ofSeq
    do if alphas.Length <> 11 then failwith "Exactly 11 alpha-cuts expected."

    member this.alphaCuts with get() = alphas |> Array.ofSeq
    member this.Bottom with get() = alphas.[0]
    member this.Top with get() = alphas.[10]
    override this.ToString() = sprintf "%A" alphas

    override x.Equals(yobj) =
        match yobj with
        | :? Fuzzy as y -> x.alphaCuts = y.alphaCuts
        | _ -> false
 
    override x.GetHashCode() = hash x.alphaCuts
    
    interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? Fuzzy as y -> compare x.alphaCuts y.alphaCuts
          | _ -> invalidArg "yobj" "cannot compare values of different types"

    static member Zero =  Fuzzy(Array.create 11 Interval.Zero)
    
    static member operation f (a:Fuzzy) (b:Fuzzy) =  Fuzzy(Seq.map2 f a.alphaCuts b.alphaCuts )
    static member map f (a:Fuzzy) =  Fuzzy(Seq.map f a.alphaCuts )

    static member (*) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a*b) x y
    static member (/) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a/b) x y
    static member (+) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a+b) x y
    static member (-) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a-b) x y
    static member (*) (x : Fuzzy, y : decimal) = Fuzzy.map (fun a-> a*y) x
    static member (/) (x : Fuzzy, y : decimal) = Fuzzy.map (fun a-> a/y) x
    static member (+) (x : Fuzzy, y : decimal) = Fuzzy.map (fun a-> a+y) x
    static member (-) (x : Fuzzy, y : decimal) = Fuzzy.map (fun a-> a-y) x
    static member (*) (x : decimal, y : Fuzzy) = y * x
    static member (/) (x : decimal, y : Fuzzy) = Fuzzy.map (fun a-> x/a) y
    static member (+) (x : decimal, y : Fuzzy) = y + x
    static member (-) (x : decimal, y : Fuzzy) = Fuzzy.map (fun a-> x-a) y
    static member pow (x : Fuzzy, p : double) = Fuzzy.map (fun a-> Interval.pow(a, p)) x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<AutoOpen>]
module Fuzzy =        
    let interval(a,b,c,d) = 
        if a>b || b>c || c>d then failwith "expected a>=b>=c>=d"
        Fuzzy(seq { for i in 0..10 -> { a = a+(b-a)*0.1m*decimal i; b = c+(d-c)*0.1m*decimal (10-i) } } )    
    
    let number(a,b,c) = interval(a,b,b,c)
    
    let point(a) = number(a,a,a)    
    
    let distance (a: Fuzzy, b: Fuzzy) = 
        let dividend = 
            Seq.zip a.alphaCuts b.alphaCuts 
            |> Seq.mapi (fun i pair -> alpha i * Interval.distance pair ) 
            |> Seq.sum
        Math.Max(0m, dividend/5.5m )
    
    let plot (x : Fuzzy) = 
        seq { for i in 0..10 -> x.alphaCuts.[i].a, alpha i 
              for i in 10..-1..0 -> x.alphaCuts.[i].b, alpha i  } |> Array.ofSeq

