
namespace Images

open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open System

type rgb = {
    mutable red: byte;
    mutable green: byte;
    mutable blue: byte;
    }

type bmp = {
    height: int;
    width : int;
    stride: int;
    bytes : byte[];
    }


module Util =
    
    ///Returns byte array from image with given filepath
    let private getImgArray (filepath:string) =
        use bmp = new Bitmap(filepath)
        let rect = new Rectangle(0, 0, bmp.Width, bmp.Height);
        let bmpData = bmp.LockBits(rect, ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);
        let ptr = bmpData.Scan0
        let bytes = bmpData.Stride * bmpData.Height;
        let array = Array.create bytes 0uy
        Marshal.Copy(ptr,array,0,bytes)
        bmp.UnlockBits(bmpData)
        array

    ///Returns array of array of rgb values of a bmp  
    let private bytesToRgb (bmp:bmp)  =
        let rgbArray = Array.init bmp.height (fun y-> (Array.init bmp.width (fun x->{red = 0uy; green = 0uy; blue = 0uy;})))
        for j in 0..(bmp.height - 1) do
            for i in 0..(bmp.width - 1) do
                rgbArray.[j].[i].blue <- bmp.bytes.[j * bmp.stride + (i * 3)]
                rgbArray.[j].[i].green <- bmp.bytes.[j * bmp.stride + (i * 3) + 1]
                rgbArray.[j].[i].red <- bmp.bytes.[j * bmp.stride + (i * 3) + 2]
        rgbArray    
        
    let private rgbToBytes stride (rgb:rgb[][]) = 
        let width = rgb.[0].Length
        let height = rgb.Length
        let bytes = Array.create (stride * height) 0uy
        for i in 0..height - 1 do
            for j in 0..width - 1 do
                let byteRowCount = i * stride
                bytes.[byteRowCount + j * 3] <- rgb.[i].[j].blue
                bytes.[byteRowCount + j * 3 + 1] <- rgb.[i].[j].green
                bytes.[byteRowCount + j * 3 + 2] <- rgb.[i].[j].red
        bytes

    let private intPtrToByteArray (bytes:byte[]) = 
        let pointer = Marshal.AllocHGlobal bytes.Length
        Marshal.Copy(bytes, 0, pointer, bytes.Length)
        pointer

    let bmpToPicture (bmp:bmp) = 
        let picture = new Bitmap(bmp.width,bmp.height,bmp.stride,PixelFormat.Format24bppRgb, intPtrToByteArray bmp.bytes)
        picture
    
    let pictureToBmp (filepath:string) = 
        use bmp = new Bitmap(filepath)
        let rect = new Rectangle(0, 0, bmp.Width, bmp.Height);
        let bmpData = bmp.LockBits(rect, ImageLockMode.ReadWrite, PixelFormat.Format24bppRgb);
        let ptr = bmpData.Scan0
        let bytes = bmpData.Stride * bmpData.Height;
        let pBuff = Marshal.AllocHGlobal(bytes)
        let array = Array.create bytes 0uy
        Marshal.Copy(ptr,array,0,bytes)
        Marshal.Copy(array,0,pBuff,bytes)
        let data = {width = bmp.Width; height = bmp.Height; stride = bmpData.Stride; bytes = array;}
        //let image = new Bitmap(bmpData.Width, bmpData.Height, bmpData.Stride, PixelFormat.Format24bppRgb,pBuff)
        bmp.UnlockBits(bmpData)
        data


    let filter xform bmp = 
        bytesToRgb bmp
        |>xform
        |>rgbToBytes bmp.stride
        |>(fun x -> {bmp with bytes = x})

        
