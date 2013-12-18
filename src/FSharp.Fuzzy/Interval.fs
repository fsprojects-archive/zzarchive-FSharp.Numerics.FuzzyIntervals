namespace FSharp.Fuzzy

open System

[<AutoOpen>]
module Utils =
    let min (xs : double seq) = Seq.fold (fun (acc : double) x -> Math.Min(acc,x)) Double.MaxValue xs
    let max (xs : double seq) = Seq.fold (fun (acc : double) x -> Math.Max(acc,x)) Double.MinValue xs
    
type Interval = 
    { a:double; b:double }
    member this.Middle = (this.a+this.b)/2.
    static member (*) (x : Interval, y : Interval) = 
        let list = [x.a * y.a; x.a * y.b; x.b * y.a; x.b * y.b]
        { a =  min list; b = max list}
    static member zeroLength (x : double) = {a=x; b=x}
    static member Zero =  Interval.zeroLength 0.
    static member (/) (x : Interval, y : Interval) = 
        if y.a < 0. && y.b > 0. then failwith "Divider cannot contain zero."
        x * { a = 1. / y.b; b = 1. / y.a}
    static member (+) (x : Interval, y : Interval) = { a = x.a + y.a; b = x.b + y.b}
    static member (-) (x : Interval, y : Interval) = { a = x.a - y.b; b = x.b - y.a}
    static member (*) (x : Interval, y : double) = x * Interval.zeroLength(y)
    static member (/) (x : Interval, y : double) = x / Interval.zeroLength(y)
    static member (+) (x : Interval, y : double) = x + Interval.zeroLength(y)
    static member (-) (x : Interval, y : double) = x - Interval.zeroLength(y)
    static member (*) (x : double, y : Interval) = Interval.zeroLength(x) * y
    static member (/) (x : double, y : Interval) = Interval.zeroLength(x) / y
    static member (+) (x : double, y : Interval) = Interval.zeroLength(x) + y
    static member (-) (x : double, y : Interval) = Interval.zeroLength(x) - y
    static member pow (x : Interval, p : double) = { a =  Math.Pow(x.a, p); b = Math.Pow(x.b, p)}
  
[<AutoOpen>]
module IntervalModule =  
    let distance (x : Interval, y : Interval) = x.Middle - y.Middle
    
