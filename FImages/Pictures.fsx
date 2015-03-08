#r @"bin\Debug\FImages.dll"

open System
open System.IO
open Images
let dir = __SOURCE_DIRECTORY__

let filepath = dir + @"\dog.JPG"
let getImage func filepath=
    use newImage =
        Util.pictureToBmp(filepath)
        //|>Util.filter (Filters.grayscale 2.0)
        |>Util.filter func
        |>Util.bmpToPicture
        
    newImage.Save(dir + @"\output.jpg")

getImage (Filters.blur 10)  filepath