namespace FSharp.Fuzzy

open System
    
type Interval = 
    { a:decimal; b:decimal }
    member this.Middle = (this.a+this.b)/2m
    static member operation f (x : Interval, y : Interval) = 
        let list = [f x.a y.a; f x.a y.b; f x.b y.a; f x.b y.b]
        let min (xs : decimal seq) = Seq.fold (fun (acc : decimal) x -> Math.Min(acc,x)) Decimal.MaxValue xs
        let max (xs : decimal seq) = Seq.fold (fun (acc : decimal) x -> Math.Max(acc,x)) Decimal.MinValue xs
        { a =  min list; b = max list}
    static member (*) (x : Interval, y : Interval) = Interval.operation (fun x y-> x*y) (x,y)
    static member zeroLength (x : decimal) = {a=x; b=x}
    static member Zero =  Interval.zeroLength 0m
    static member (/) (x : Interval, y : Interval) = 
        if y.a < 0m && y.b > 0m then failwith "Divider cannot contain zero."
        Interval.operation (fun x y-> x/y) (x,y)
    static member (+) (x : Interval, y : Interval) = { a = x.a + y.a; b = x.b + y.b}
    static member (-) (x : Interval, y : Interval) = { a = x.a - y.b; b = x.b - y.a}
    static member (*) (x : Interval, y : decimal) = x * Interval.zeroLength(y)
    static member (/) (x : Interval, y : decimal) = x / Interval.zeroLength(y)
    static member (+) (x : Interval, y : decimal) = x + Interval.zeroLength(y)
    static member (-) (x : Interval, y : decimal) = x - Interval.zeroLength(y)
    static member (*) (x : decimal, y : Interval) = Interval.zeroLength(x) * y
    static member (/) (x : decimal, y : Interval) = Interval.zeroLength(x) / y
    static member (+) (x : decimal, y : Interval) = Interval.zeroLength(x) + y
    static member (-) (x : decimal, y : Interval) = Interval.zeroLength(x) - y
    static member pow (x : Interval, p : double) = { a =  Math.Pow(double x.a, p) |> decimal; b = Math.Pow(double x.b, p) |> decimal}
    static member distance (x : Interval, y : Interval) = x.Middle - y.Middle
