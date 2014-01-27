namespace FSharp.Fuzzy

open System

/// Fuzzy type
/// This type implements subset of fuzzy intervals described by convex membership function $\mu$ with values changing from 0 to 1. 
/// Accepts array of intervals comprising $\alpha$-cuts, starting from `Bottom` level up to the `Top`.    
[<StructuredFormatDisplayAttribute("{alphaCuts}")>]
type Fuzzy(a : Interval seq) = 
    let alphas = a |> Array.ofSeq

    ///Exposes raw list of $\alpha$-cuts
    member this.alphaCuts with get() = alphas |> Array.ofSeq
    ///$\alpha$-cut with $\mu$ = 0 
    member this.Bottom with get() = alphas.[0]
    ///$\alpha$-cut with $\mu$ = 1 
    member this.Top with get() = alphas.[alphas.Length - 1]
    
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

    ///Fuzzy set comprized from 11 zero $\alpha$-cuts 
    static member Zero =  Fuzzy(Array.create 11 Interval.Zero)
    
    ///Generic binary operation over two fuzzy sets implemented as a sum of operations over each $\alpha$-cut
    static member operation f (a:Fuzzy) (b:Fuzzy) =  Fuzzy(Seq.map2 f a.alphaCuts b.alphaCuts )
    ///Generic unary operation overa set implemented as an application of `f` on each $\alpha$-cut
    static member map f (a:Fuzzy) =  Fuzzy(Seq.map f a.alphaCuts )
    
    /// Multiply `x` by `y`, uses `operation`
    static member (*) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a*b) x y
    /// Divide `x` by `y`, uses `operation`
    static member (/) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a/b) x y
    /// Add `x` to `y`, uses `operation`
    static member (+) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a+b) x y
    /// Subtract `y` from `x`, uses `operation`
    static member (-) (x : Fuzzy, y : Fuzzy) = Fuzzy.operation (fun a b-> a-b) x y
    /// Multiply `x` by `y`, uses `map`
    static member (*) (x : Fuzzy, y : decimal) = Fuzzy.map (fun a-> a*y) x
    /// Divide `x` by `y`, uses `map`
    static member (/) (x : Fuzzy, y : decimal) = Fuzzy.map (fun a-> a/y) x
    /// Add `x` to `y`, uses `map`
    static member (+) (x : Fuzzy, y : decimal) = Fuzzy.map (fun a-> a+y) x
    /// Subtract `y` from `x`, uses `map`
    static member (-) (x : Fuzzy, y : decimal) = Fuzzy.map (fun a-> a-y) x
    /// Multiply `x` by `y`, uses `map`
    static member (*) (x : decimal, y : Fuzzy) = y * x
    /// Divide `x` by `y`, uses `map`
    static member (/) (x : decimal, y : Fuzzy) = Fuzzy.map (fun a-> x/a) y
    /// Add `x` to `y`, uses `map`
    static member (+) (x : decimal, y : Fuzzy) = y + x
    /// Subtract `y` from `x`, uses `map`
    static member (-) (x : decimal, y : Fuzzy) = Fuzzy.map (fun a-> x-a) y
    /// Rises `x` to the power of `p`, uses `map`
    static member pow (x : Fuzzy, p : double) = Fuzzy.map (fun a-> Interval.pow(a, p)) x

/// This module contains bunch of factory methods for Fuzzy type
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<AutoOpen>]
module Fuzzy =
    /// Converts number of the level in $\alpha$-cut array to the value of membership function $\mu$
    let alpha total level = if level = 0 then 0m else 1.m / decimal (total - 1) * decimal level

    ///Creates trapezoid fuzzy set with bottom $\alpha$-cut `{a,d}` and top $\alpha$-cut `{b,c}`         
    let trapezoid levels (a,b,c,d) = 
        let maxIndex = levels - 1
        if a>b || b>c || c>d then failwith "expected a>=b>=c>=d"
        Fuzzy(seq { for i in 0..maxIndex -> { a = a+(b-a)*0.1m*decimal i; b = c+(d-c)*0.1m*decimal (maxIndex-i) } } )    
   
    ///Creates trapezoid with 11 $\alpha$-cuts which gives increment of 0.1 in $\mu$ from one $\alpha$-cut to the next
    let interval(a,b,c,d) = trapezoid 11 (a,b,c,d)
         
    ///Creates triangular fuzzy set with bottom $\alpha$-cut `{a,c}` and zero-length top $\alpha$-cut `b`         
    let number(a,b,c) = interval(a,b,b,c)
    
    ///Creates fuzzy representation of zero-length interval 
    let point(a) = number(a,a,a)   
    
    ///Binary function for defuzzification of the result of operation `f` applied to each pair of corresponding $\alpha$-cuts taken 
    ///from `a` and `b` weighted by value of $\mu$.
    ///For example, useful for calcualtion of the `distance` between two fuzzy sets:
    ///${\Delta }_{\bar{A}-\bar{B}} = \frac{\sum_{\alpha}\alpha \Delta_{A_\alpha-B_\alpha}}{\sum_{\alpha}\alpha}$
    ///*Important!* Only works for fuzzy sets with the same number of $\alpha$-cuts
    let binary f (a: Fuzzy) (b: Fuzzy) = 
        assert (a.alphaCuts.Length = b.alphaCuts.Length)
        let result = 
            Seq.zip a.alphaCuts b.alphaCuts 
            |> Seq.mapi (fun i pair -> alpha a.alphaCuts.Length i * f pair ) 
            |> Seq.sum
        result/5.5m 

    ///Unary function for defuzzification of the result of operation `f` 
    ///applied to each $\alpha$-cut of `a` weighted by corresponding value of $\mu$. 
    ///$ W f = \frac{\sum_{\alpha}\alpha f}{\sum_{\alpha}\alpha}$
    let unary f (a: Fuzzy) = 
        let result = 
            a.alphaCuts 
            |> Seq.mapi (fun i b -> alpha a.alphaCuts.Length i * f b ) 
            |> Seq.sum
        result/5.5m 
    
    ///Calculates distance between two fuzzy sets `a` and `b`
    let distance a b = binary Interval.distance a b
    ///Calculates weighted width of fuzzy set `a`
    let width a = unary (fun i->i.b - i.a) a
    ///Calculates weighted risk of fuzzy set `a`, i.e. the ratio between its width and the middle on each $\alpha$-cut
    let risk a = unary (fun i->2m * (i.b - i.a)/(i.a+i.b)) a
    ///Represents fuzzy set `a` for drawing a chart  
    let plot (a : Fuzzy) = 
        let length =  a.alphaCuts.Length - 1
        seq { for i in 0..length -> a.alphaCuts.[i].a, alpha a.alphaCuts.Length i 
              for i in length .. -1 .. 0 -> a.alphaCuts.[i].b, alpha a.alphaCuts.Length i  } |> Array.ofSeq

