#load "Library.fs"
open FSharp.Fuzzy

let i1 = Fuzzy.number(0.0011,0.0012,0.0014)
let i2 = Fuzzy.number(0.0008,0.0011,0.0016)
let M = 1000.
let couponRate = 0.1

let coupon = M * couponRate
let presentValue = coupon/(1.+i1)+(coupon + M)/Fuzzy.pow(1.+i2, 2.)
