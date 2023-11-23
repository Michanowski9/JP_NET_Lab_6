namespace MyProject


module MyModule =
    #light
    let y = 0;

    let data = [1.;2.;3.;4.]

    let sqr (x: float) = x * x

    let sumOfSquaresI nums = 
        let mutable acc = 0.
        for x in nums do
            acc <- acc + sqr x
        acc

    let rec sumOfSquaresF nums = 
        match nums with
        | [] -> 0.
        | h::t -> sqr h + sumOfSquaresF t

    let sumOfSquares nums = 
        nums
        |> Seq.map(fun (x:float) -> x * x)
        |> Seq.sum

    #r "C:/<path/to/nuget>/.nuget/packages/fsharp.collections.parallelseq/1.2.0/lib/netstandard2.0/FSharp.Collections.ParallelSeq.dll"
    open FSharp.Collections.ParallelSeq
    let sumOfSquaresP nums = 
        nums
        |> PSeq.map(fun (x:float) -> x * x)
        |> PSeq.sum

    open System.Net
    open System.IO
    let ticker = "MSFT"
    let url = "https://query1.finance.yahoo.com/v7/finance/download/" + ticker + "?period1=1542734207&period2=1574270207&interval=1d&events=history&crumb=cGtjCJ1cWt/"
    let req = WebRequest.Create(url) :?> HttpWebRequest
    let resp = downcast req.GetResponse() :> HttpWebResponse
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let csv = reader.ReadToEnd()

    let prices =
         csv.Split([|'\n'|])
         |> Seq.skip 1
         |> Seq.map (fun line -> line.Split([|','|]))
         |> Seq.filter(fun values -> values |> Seq.length = 7)
         |> Seq.map (fun values ->
         System.DateTime.Parse(values.[0]),
         float values.[6])

    #load "<path/to/nuget>/.nuget/packages/fsharp.charting/X.X.X/FSharp.Charting.fsx"
    open FSharp.Charting
    Chart.Line(prices).ShowChart()