#r @"bin\Debug\FImages.dll"
open System
open Images

let p1 = Point.Create 1. 0.    
let p2 = Point.Create -1. 0.

let redBlue value = 
    let absValue = abs (float value)
    if value <= 0. then 
        {red=byte (255. * absValue);  green=0uy; blue=0uy}
    else
        {red=0uy;  green=0uy; blue=byte (255. * absValue)}

let sample s = Point.Distance p2 s - Point.Distance p1 s

let graph = Graph.createGraph 900 900 3 3 (fun x y -> 
     let s = sample (Point.Create x y)
     floor (s / (Point.Distance p1 p2) * 12.)
     )

let graph2 = Graph.createGraphParallel 1000 1000 3 3 (fun x y -> 
     let s = sample (Point.Create x y)
     floor (s / (Point.Distance p1 p2) * 10.)
     )

Graph.graphToPicture (__SOURCE_DIRECTORY__ + @"/Graph.bmp") redBlue graph2

