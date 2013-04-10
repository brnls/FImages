
#r @"E:\GitHub\FImages\FImages\bin\Debug\FImages.dll"

open System
//open MSDN.FSharp.Charting
//open System.Windows.Forms.DataVisualization
//open Convolve
open System.IO
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Images
#time


let rand = new Random()
let filepath = @"C:\Users\Brian\Desktop\" + "dog.JPG"
let getImage func filepath=
    use newImage =
        Util.pictureToBmp(filepath)
        //|>Util.filter (Filters.grayscale 2.0)
        |>Util.filter func
        |>Util.bmpToPicture
        
    newImage.Save(@"C:\Users\Brian\Desktop\" + "test2.jpg")

getImage (Filters.smear) filepath


//Util.pictureToBmp(filepath)
//|>Util.filter (Filters.grayscale 2.0)
//|>Util.filter (Filters.edge 1)
//|>Util.bmpToPicture
//|>(fun x-> x.Save(@"C:\Users\Brian\Desktop\" + "test2.jpg"))