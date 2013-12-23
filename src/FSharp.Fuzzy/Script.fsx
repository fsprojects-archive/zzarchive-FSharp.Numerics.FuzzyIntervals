#load "Interval.fs"
open FSharp.Fuzzy

#load "Fuzzy.fs"
#load "../../packages/FSharp.Charting.0.90.5/FSharp.Charting.fsx"
open FSharp.Charting
open FSharp.Fuzzy

let i1 = Fuzzy.number(0.0011m,0.0012m,0.0014m)
let i2 = Fuzzy.number(0.0008m,0.0011m,0.0016m)
let M = 1000m
let couponRate = 0.1m

let coupon = M * couponRate
let presentValue = coupon/(1m+i1)+(coupon + M)/Fuzzy.pow(1m+i2, 2.)
plot presentValue |> Chart.Line

