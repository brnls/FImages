namespace Images

open System
open Images

type Point = {x:float; y:float}
    with 
    static member Create a b = {x=a;y=b} 

    static member Distance point1 point2 = 
        sqrt <| (point1.x - point2.x)**2. + (point1.y - point2.y)**2.

module Graph = 

    let createGraph xRes yRes xLength yLength func = 
        let graph = Array.init yRes (fun x -> Array.init xRes (fun y -> 0.))
        for i in 0 .. graph.Length - 1 do
            for j in 0 .. graph.[i].Length - 1 do
                let xCoord = ((float j)/(float xRes)) * (float xLength * 2.) - (float xLength)
                let yCoord = (float yLength) - ((float i)/(float yRes)) * (float yLength * 2.)
                graph.[i].[j] <- func xCoord yCoord
        graph

    let createGraphParallel xRes yRes xLength yLength func = 
        let graph = Array.init yRes (fun x -> Array.init xRes (fun y -> 0.))
        Array.Parallel.iteri(fun i x ->
                for j in 0 .. graph.[i].Length - 1 do
                    let xCoord = ((float j)/(float xRes)) * (float xLength * 2.) - (float xLength)
                    let yCoord = (float yLength) - ((float i)/(float yRes)) * (float yLength * 2.)
                    graph.[i].[j] <- func xCoord yCoord
                ) graph
        graph

    let graphToRgb (xForm:float -> rgb) (graph : float[][]) =
        Array.Parallel.map(fun y -> Array.map(fun x -> xForm x) y) graph

    let normalize (arr:float[][]) = 
        let mutable maxValue = 0.
        for i in 0 .. arr.Length - 1 do
            for j in 0 .. arr.[i].Length - 1 do
                if abs (arr.[i].[j]) > maxValue then maxValue <- abs arr.[i].[j]

        for i in 0 .. arr.Length - 1 do
            for j in 0 .. arr.[i].Length - 1 do
                arr.[i].[j] <- arr.[i].[j] / maxValue

    let graphToPicture fileName xForm graph= 
        normalize graph
        let rgb = graphToRgb xForm graph
        Util.rgbArrayToPicture rgb fileName

