namespace FSharp.Fuzzy

open System
    
type Interval = 
    { a:double; b:double }
    static member (*) (x : Interval, y : Interval) = 
        let list = [x.a * y.a; x.a * y.b; x.b * y.a; x.b * y.b]
        let min (xs : double list) = Seq.fold (fun (acc : double) x -> Math.Min(acc,x)) Double.MaxValue xs
        let max (xs : double list) = Seq.fold (fun (acc : double) x -> Math.Max(acc,x)) Double.MinValue xs
        { a =  min list; b = max list}
    static member (/) (x : Interval, y : Interval) = { a = x.a / y.b; b = x.b / y.a}
    static member (+) (x : Interval, y : Interval) = { a = x.a + y.a; b = x.b + y.b}
    static member (-) (x : Interval, y : Interval) = { a = x.a - y.b; b = x.b - y.a}
    static member (*) (x : Interval, y : double) = { a = x.a * y; b = x.b * y}
    static member (/) (x : Interval, y : double) = { a = x.a / y; b = x.b / y}
    static member (+) (x : Interval, y : double) = { a = x.a + y; b = x.b + y}
    static member (-) (x : Interval, y : double) = { a = x.a - y; b = x.b - y}
    static member (*) (x : double, y : Interval) = { a = x * y.a; b = x * y.b}
    static member (/) (x : double, y : Interval) = { a = x / y.b; b = x / y.a}
    static member (+) (x : double, y : Interval) = { a = x + y.a; b = x + y.b}
    static member (-) (x : double, y : Interval) = { a = x - y.b; b = x - y.a}
    static member pow (x : Interval, p : double) = { a =  Math.Pow(x.a, p); b = Math.Pow(x.b, p)}
    static member zeroLength (x : double) = {a=x; b=x} 

