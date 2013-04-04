
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
open Images.Filters
open Images.Util
#time


let rand = new Random()
let filepath = @"C:\Users\Brian\Desktop\" + "dog.JPG"
let getImage func filepath=
    use newImage =
        pictureToBmp(filepath)
        |>Images.Util.filter func
        |>bmpToPicture
        
    newImage.Save(@"C:\Users\Brian\Desktop\" + "test2.jpg")

getImage (blur 2) filepath
