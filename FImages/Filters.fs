namespace Images

open System

module Filters =

    let private cap value = 
            if value > 255.0 then 255uy
            else byte value

    ///One to one mapping of all values of rgb array to another using the function rgbXform
    let private mapRgb rgbXform rgbArray =   
        rgbArray
        |>Array.map(fun x->
            x
            |>Array.map(fun y -> rgbXform y))

    ///Mapping of one pixel and its immediate surrounding neighbors to one pixel
    let private mapConvolveNeighborRgb neighborXForm (rgbArray:Images.rgb[][]) =
        let filteredRgb = Array.init rgbArray.Length (fun y-> (Array.init rgbArray.[0].Length (fun x->{red = 0uy; green = 0uy; blue = 0uy;})))
        for i in 1..rgbArray.Length - 2 do
            for j in 1..rgbArray.[0].Length - 2 do
                neighborXForm i j filteredRgb rgbArray 
        filteredRgb

    ///Blurs image by making colors averages of their nearest neighbor
    let blur scale rgbArray =
        let operation centerX centerY (destination:Images.rgb[][]) (source:Images.rgb[][]) =
            let mutable sumR = 0
            let mutable sumG = 0
            let mutable sumB = 0
            for i in centerX-1..centerX+1 do
                for j in centerY-1..centerY+1 do
                    sumR <- sumR + int source.[i].[j].red
                    sumG <- sumG + int source.[i].[j].green
                    sumB <- sumB + int source.[i].[j].blue
            destination.[centerX].[centerY].red <- byte (sumR/9)
            destination.[centerX].[centerY].green <- byte (sumG/9)
            destination.[centerX].[centerY].blue <- byte (sumB/9)
        let rec iterate count rgbVals = 
            match count with
            |0 -> rgbVals
            |_ -> iterate (count - 1) (rgbVals |> mapConvolveNeighborRgb operation)
        iterate scale rgbArray

        

    ///Grayscale picture. Brightness variable, determined by scale parameter     
    let grayscale scale rgbArray =
        let operation rgb = 
            let average = ((scale * 0.3 * (float)rgb.red + scale * 0.59* (float) rgb.green + scale * 0.11 * (float)rgb.blue)/3.0)
            if average > 255.0 then 
                {red = 255uy; green = 255uy; blue = 255uy;}
            else
                {red = byte average; green = byte average; blue = byte average;}
        rgbArray |> mapRgb operation
    
    ///Pass a tuple of (bool,bool,bool) which will set the respective (r,g,b) from the pixel rgb to 0
    let removeRgb remove rgbArray =
        let operation rgb = 
            let r,g,b = remove
            let boolTo0 value bool =
                match bool with
                |true -> value
                |false -> 0uy
            {red = boolTo0 rgb.red r; green = boolTo0 rgb.green g; blue = boolTo0 rgb.blue b;}
        mapRgb operation rgbArray
    ///Add a different random value from 0 to [scale] to each of the r, g, b values
    let addNoise (rand:Random) scale rgbArray = 
        let operation rgb = 
            let r,g,b = (scale * rand.NextDouble(),scale * rand.NextDouble(),scale * rand.NextDouble())
            {red = cap(float rgb.red + r); green = cap(float rgb.green + g); blue = cap(float rgb.blue + b);}
        mapRgb operation rgbArray
    
    ///Add a single random value from 0 to [scale] to each rgb value
    let addNoiseUniform (rand:Random) scale rgbArray =
        let operation rgb = 
            let randNum = rand.NextDouble() * scale
            {red = cap(float rgb.red + randNum); green = cap(float rgb.green + randNum); blue = cap(float rgb.blue + randNum);}
        mapRgb operation rgbArray

    /// Get highest value of r,g,b and set the other two values to 0. If there is a tie, each one in the tie is set to maximum value.
    let oneToRuleThemAll rgbArray =
        let operation rgb = 
            let max =
                rgb.red
                |>(fun x-> if rgb.green >= x then rgb.green else x)
                |>(fun x-> if rgb.blue >= x then rgb.blue else x)

            let maxOr0 value = 
                if value < max then 0uy else max
            {red = maxOr0 rgb.red; green = maxOr0 rgb.green; blue = maxOr0 rgb.blue;}
        mapRgb operation rgbArray
    
    ///Brings out brighter colors (possibly more than one) of each pixel while causing others to diminish
    let saturation scale rgbArray = 
        let operation rgb = 
            let mutable r = Math.Pow(float rgb.red / 200.0, scale) * float rgb.red
            let mutable g = Math.Pow(float rgb.green / 200.0, scale) * float rgb.green
            let mutable b = Math.Pow(float rgb.blue / 200.0, scale) * float rgb.blue
            {red = byte (cap r); green = byte (cap g); blue = byte (cap b);}
        mapRgb operation rgbArray
    
    ///Makes dark darker and light lighter
    let contrast scale rgbArray = 
        let operation rgb = 
            let weight = Math.Pow((float rgb.red + float rgb.blue + float rgb.green)/375.0, scale)
            let mutable r = weight * float rgb.red
            let mutable g = weight * float rgb.green
            let mutable b = weight * float rgb.blue
            {red = byte (cap r); green = byte (cap g); blue = byte (cap b);}
        mapRgb operation rgbArray
    
    ///For each pixel, makes the dominant color more dominant, and makes the lesser colors recede
    let strongestColor scale rgbArray = 
        let operation rgb = 
            let max =
                rgb.red
                |>(fun x-> if rgb.green >= x then rgb.green else x)
                |>(fun x-> if rgb.blue >= x then rgb.blue else x)
            let mutable r = Math.Pow(float rgb.red / float max, scale) * float rgb.red
            let mutable g = Math.Pow(float rgb.green / float max, scale) * float rgb.green
            let mutable b = Math.Pow(float rgb.blue / float max, scale) * float rgb.blue
            {red = byte (cap r); green = byte (cap g); blue = byte (cap b);}
        mapRgb operation rgbArray