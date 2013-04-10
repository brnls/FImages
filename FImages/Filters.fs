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

    ///Mapping of one gridSize * gridSize square from source image to center pixel on new image. gridSize must be odd
    let private mapConvolveNeighborRgb gridSize neighborXForm (rgbArray:Images.rgb[][]) =
        if gridSize % 2 = 0 then failwith("gridSize must be odd")
        let filteredRgb = Array.init rgbArray.Length (fun y-> (Array.init rgbArray.[0].Length (fun x->{red = 0uy; green = 0uy; blue = 0uy;})))
        let gridPadding = gridSize/2
        for i in gridPadding..(rgbArray.Length - gridPadding - 1) do
            for j in gridPadding..(rgbArray.[0].Length - gridPadding - 1) do
                neighborXForm i j filteredRgb rgbArray 
        filteredRgb

    let private mapLine smearXForm (rgbArray:Images.rgb[][]) = 
        let filteredRgb = Array.init rgbArray.Length (fun y-> (Array.init rgbArray.[0].Length (fun x->{red = 0uy; green = 0uy; blue = 0uy;})))
        for i in 0..rgbArray.Length - 21 do
            for j in 0..rgbArray.[0].Length - 21 do
                smearXForm i j filteredRgb rgbArray
        filteredRgb

    let smear rgbArray = 
        let operation positionX positionY (destination:Images.rgb[][]) (source:Images.rgb[][]) =
            let parentPixR = source.[positionX].[positionY].red
            let parentPixG = source.[positionX].[positionY].green
            let parentPixB = source.[positionX].[positionY].blue
            for i in 0..19 do
                destination.[positionX + i].[positionY].red <- byte (float parentPixR * (0.5 - 0.05 * (float i )) + float destination.[i].[positionY].red * ( float i * 0.05))
                destination.[positionX + i].[positionY].green <- byte (float parentPixG * (0.5 - 0.05 * (float i)) + float destination.[i].[positionY].green * (float i * 0.05))
                destination.[positionX + i].[positionY].blue <- byte (float parentPixB * (0.5 - 0.05 * (float i)) +  float destination.[i].[positionY].blue * (float i * 0.05))
        rgbArray |> mapLine operation

    ///Brings out edges. Best used on colors with simple pictures, low noise.
    let edgeFinder rgbArray =
        let operation centerX centerY (destination:Images.rgb[][]) (source:Images.rgb[][]) =
            let mutable sumR = 0
            let mutable sumG = 0
            let mutable sumB = 0
            let centerParity = abs((centerX - centerY) % 2)
            for i in centerX-1..centerX+1 do
                for j in centerY-1..centerY+1 do
                    let parity = abs((i - j) % 2)
                    if i = centerX && j = centerY then
                        sumR <- sumR + 12 * int source.[i].[j].red
                        sumG <- sumG + 12 * int source.[i].[j].green
                        sumB <- sumB + 12 * int source.[i].[j].blue
                    else if parity <> centerParity then
                        sumR <- sumR - 2 * int source.[i].[j].red
                        sumG <- sumG - 2 * int source.[i].[j].green
                        sumB <- sumB - 2 * int source.[i].[j].blue
                    else 
                        sumR <- sumR - 1 * int source.[i].[j].red
                        sumG <- sumG - 1 * int source.[i].[j].green
                        sumB <- sumB - 1 * int source.[i].[j].blue

            destination.[centerX].[centerY].red <- byte (cap (float sumR))
            destination.[centerX].[centerY].green <- byte (cap (float sumG))
            destination.[centerX].[centerY].blue <- byte (cap (float sumB))
        rgbArray |> mapConvolveNeighborRgb 3 operation

    ///Blur image. Increase scale for more blurring of image.
    let blur scale rgbArray =
        let operation centerX centerY (destination:Images.rgb[][]) (source:Images.rgb[][]) =
            let mutable sumR = 0
            let mutable sumG = 0
            let mutable sumB = 0
            for i in centerX-2..centerX+2 do
                for j in centerY-2..centerY+2 do
                    sumR <- sumR + 1 * int source.[i].[j].red
                    sumG <- sumG + 1 * int source.[i].[j].green
                    sumB <- sumB + 1 * int source.[i].[j].blue

            destination.[centerX].[centerY].red <- byte (sumR/25)
            destination.[centerX].[centerY].green <- byte (sumG/25)
            destination.[centerX].[centerY].blue <- byte (sumB/25)
        let rec iterate count rgbVals = 
            match count with
            |0 -> rgbVals
            |_ -> iterate (count - 1) (rgbVals |> mapConvolveNeighborRgb 5 operation)
        iterate scale rgbArray

    ///Blur image softly. Increase scale for more blurring of image.
    let softBlur scale rgbArray : rgb[][] =
        let operation centerX centerY (destination:Images.rgb[][]) (source:Images.rgb[][]) =
            let mutable sumR = 0
            let mutable sumG = 0
            let mutable sumB = 0
            for i in centerX-1..centerX+1 do
                for j in centerY-1..centerY+1 do
                    if i = centerX && j = centerY then
                        sumR <- sumR + 3 * int source.[i].[j].red
                        sumG <- sumG + 3 * int source.[i].[j].green
                        sumB <- sumB + 3 * int source.[i].[j].blue
                    else
                        sumR <- sumR + 1 * int source.[i].[j].red
                        sumG <- sumG + 1 * int source.[i].[j].green
                        sumB <- sumB + 1 * int source.[i].[j].blue
            destination.[centerX].[centerY].red <- byte (sumR/11)
            destination.[centerX].[centerY].green <- byte (sumG/11)
            destination.[centerX].[centerY].blue <- byte (sumB/11)
        let rec iterate count rgbVals = 
            match count with
            |0 -> rgbVals
            |_ -> iterate (count - 1) (rgbVals |> mapConvolveNeighborRgb 3 operation)
        iterate scale rgbArray


    let wind scale rgbArray : rgb[][] =
        let operation centerX centerY (destination:Images.rgb[][]) (source:Images.rgb[][]) =
            let mutable sumR = 0
            let mutable sumG = 0
            let mutable sumB = 0
            for i in centerX-1..centerX+1 do
                for j in centerY-1..centerY+1 do
                    if i = centerX - 1 && j = centerY - 1 then
                        sumR <- sumR + 3 * int source.[i].[j].red
                        sumG <- sumG + 3 * int source.[i].[j].green
                        sumB <- sumB + 3 * int source.[i].[j].blue
                    else
                        sumR <- sumR + 1 * int source.[i].[j].red
                        sumG <- sumG + 1 * int source.[i].[j].green
                        sumB <- sumB + 1 * int source.[i].[j].blue
            destination.[centerX - 1].[centerY].red <- byte (sumR/11)
            destination.[centerX - 1].[centerY].green <- byte (sumG/11)
            destination.[centerX - 1].[centerY].blue <- byte (sumB/11)
        let rec iterate count rgbVals = 
            match count with
            |0 -> rgbVals
            |_ -> iterate (count - 1) (rgbVals |> mapConvolveNeighborRgb 3 operation)
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