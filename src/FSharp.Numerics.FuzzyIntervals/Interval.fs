namespace FSharp.Numerics.FuzzyIntervals

open System

/// Interval record
///     
///      let i = {a = 1m; b = 2m }
///      
/// `a` is expected to be less or equal `b`         
type Interval = 
    {
        ///Lower bound 
        a:decimal
        ///Upper bound
        b:decimal 
    }
    /// Middle point between `a` and `b`
    member this.Middle = (this.a+this.b)/2m
    /// Generic binary operation over two intervals compliant with pivotal rule of interval mathematics - 
    /// operation should result in widest possible interval
    static member operation f (x : Interval, y : Interval) = 
        let list = [f x.a y.a; f x.a y.b; f x.b y.a; f x.b y.b]
        let min (xs : decimal seq) = Seq.fold (fun (acc : decimal) x -> Math.Min(acc,x)) Decimal.MaxValue xs
        let max (xs : decimal seq) = Seq.fold (fun (acc : decimal) x -> Math.Max(acc,x)) Decimal.MinValue xs
        { a =  min list; b = max list}
    /// Multiply `x` by `y`, uses `operation`
    static member (*) (x : Interval, y : Interval) = Interval.operation (fun x y-> x*y) (x,y)
    /// Creates interval of zero length
    static member zeroLength (x : decimal) = {a=x; b=x}
    /// Interval with `a` and `b` equal zero
    static member Zero =  Interval.zeroLength 0m
    /// Divide `x` by `y`, uses `operation`, 'y` cannot contain zero
    static member (/) (x : Interval, y : Interval) = 
        if y.a < 0m && y.b > 0m then failwith "Divider cannot contain zero."
        Interval.operation (fun x y-> x/y) (x,y)
    /// Add `x` to `y`, implemetation is simplified for performance reasons as it is easy to proof that for any intervals
    /// `a'=x.a+y.a` and `b'=x.b+y.b` gives widest possible resulting interval `a',b'` 
    static member (+) (x : Interval, y : Interval) = { a = x.a + y.a; b = x.b + y.b}
    /// Subtract `y` from `x`, implemetation is simplified for performance reasons as it is easy to proof that for any intervals
    /// `a'=x.a-y.b` and `b'=x.b-y.a` gives widest possible resulting interval `a',b'` 
    static member (-) (x : Interval, y : Interval) = { a = x.a - y.b; b = x.b - y.a}
    /// Multiply interval `x` by decimal `y`, decimal is represented as zero-length interval
    static member (*) (x : Interval, y : decimal) = x * Interval.zeroLength(y)
    /// Divide interval `x` by decimal `y`, decimal is represented as zero-length interval
    static member (/) (x : Interval, y : decimal) = x / Interval.zeroLength(y)
    /// Add interval `x` by decimal `y`, decimal is represented as zero-length interval
    static member (+) (x : Interval, y : decimal) = x + Interval.zeroLength(y)
    /// Subtract decimal `y` from interval `x`, decimal is represented as zero-length interval
    static member (-) (x : Interval, y : decimal) = x - Interval.zeroLength(y)
    /// Multiply interval `x` by decimal `y`, decimal is represented as zero-length interval
    static member (*) (x : decimal, y : Interval) = Interval.zeroLength(x) * y
    /// Divide interval `x` by decimal `y`, decimal is represented as zero-length interval
    static member (/) (x : decimal, y : Interval) = Interval.zeroLength(x) / y
    /// Add interval `x` by decimal `y`, decimal is represented as zero-length interval
    static member (+) (x : decimal, y : Interval) = Interval.zeroLength(x) + y
    /// Subtract decimal `y` from interval `x`, decimal is represented as zero-length interval
    static member (-) (x : decimal, y : Interval) = Interval.zeroLength(x) - y
    /// Rises `x` to the power of `p`
    static member pow (x : Interval, p : double) = { a =  double x.a ** p |> decimal; b = double x.b ** p |> decimal }
    /// Distance between `x` by `y` calculated as distance between their middle points
    static member distance (x : Interval, y : Interval) = Math.Abs(y.Middle - x.Middle)
