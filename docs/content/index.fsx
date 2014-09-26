(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/"

(**
F# Fuzzy Library
===================

Documentation

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The F# Fuzzy library can be <a href="https://nuget.org/packages/FSharp.Numerics.FuzzyIntervals">installed from NuGet</a>:
      <pre>PM> Install-Package FSharp.Numerics.FuzzyIntervals</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

<img src="img/logo.png" alt="F# Project" style="float:right;width:150px;margin:10px" />

Example
-------

Fuzzy valuation of 2-year bond with 10% coupon.

*)
#r "FSharp.Numerics.FuzzyIntervals.dll"
open FSharp.Numerics.FuzzyIntervals

let i1 = Fuzzy.number(0.0011m,0.0012m,0.0014m)
let i2 = Fuzzy.number(0.0008m,0.0011m,0.0016m)
let M = 1000m
let couponRate = 0.1m

let coupon = M * couponRate
let presentValue = coupon/(1m + i1) + (coupon + M)/Fuzzy.pow(1m + i2, 2.)


(**
Samples & documentation
-----------------------

The library comes with documentation. 

 * [Tutorial](tutorial.html)

 * [API Reference](reference/index.html)
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/FSharp.Numerics.FuzzyIntervals/tree/master/docs/content
  [gh]: https://github.com/fsprojects/FSharp.Numerics.FuzzyIntervals
  [issues]: https://github.com/fsprojects/FSharp.Numerics.FuzzyIntervals/issues
  [readme]: https://github.com/fsprojects/FSharp.Numerics.FuzzyIntervals/blob/master/README.md
  [license]: https://github.com/fsprojects/FSharp.Numerics.FuzzyIntervals/blob/master/LICENSE.txt
*)
