#load "Interval.fs"
open FSharp.Fuzzy

#load "Fuzzy.fs"
open FSharp.Fuzzy

let i1 = Fuzzy.number(0.0011m,0.0012m,0.0014m)
let i2 = Fuzzy.number(0.0008m,0.0011m,0.0016m)
let M = 1000m
let couponRate = 0.1m

let coupon = M * couponRate
let presentValue = coupon/(1m+i1)+(coupon + M)/Fuzzy.pow(1m+i2, 2.)

#r "System.Windows.Forms.DataVisualization.dll"
#I "../../packages/FSharp.Charting.0.90.5/lib/net40/"
#r "FSharp.Charting.dll"

open FSharp.Charting
open FSharp.Charting.ChartTypes
open System.Drawing
open System.Windows.Forms

let chart = Chart.Line([0,0;1,0;2,1;3,1;5,0; 6,0])
                    .WithYAxis(Title = "μ", 
                                TitleAlignment = StringAlignment.Far, 
                                Max = 1.,Min = 0., 
                                TitleFontSize = 20.,
                                MajorGrid = Grid(Interval=0.1))
                    .WithXAxis(Title = "x", 
                                TitleAlignment = StringAlignment.Far, 
                                TitleFontSize = 20., 
                                Min = 0., 
                                MajorGrid = Grid(false))
let ctrl = new ChartControl(chart, Dock=DockStyle.Fill)
let chartObj = ctrl.Controls |> Seq.cast<Control> |> Seq.pick (function | :? DataVisualization.Charting.Chart as x -> Some x | _-> None)
let yAxis = chartObj.ChartAreas.[0].AxisY 
yAxis.TextOrientation <- DataVisualization.Charting.TextOrientation.Horizontal
yAxis.LabelStyle.Interval <- 0.1
let form = new Form(Visible = true, TopMost = true, Width = 400, Height = 300)
form.Controls.Add(ctrl)
form.Show()

