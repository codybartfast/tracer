module Canvas

open System.Text

open Tuple

let newline = "\n"
let black = color 0.0 0.0 0.0

type Canvas (width, height) =
    let depth = 255

    let pixels = Array2D.create width height black
    let maxX = width - 1
    let maxY = height - 1

    member _.Width = width
    member _.Height = height
    member _.Pixels =
        seq{
            for y in 0..maxY do
                for x in 0..maxX do
                    yield pixels.[x, y]
        }
    member _.Item
        with get(x, y) = pixels.[x, y]
        and set(x, y) v = pixels.[x, y] <- v
    member _.Row y = [| 0..maxX |] |> Array.map (fun x -> pixels.[x, y])

    member this.ToPpm () =
        let scale depth = ((*) (float depth)) >> ((+) 0.5) >> int
        let clamp low high = (max low) >> (min high)
        let pixelValues px =
            [| red px; green px; blue px |]
            |> Array.map (scale depth >> clamp 0 depth >> string)
        let rowValues = this.Row >> Array.collect pixelValues
        let pixelData = [0..maxY] |> List.map (rowValues >> (String.concat " "))

        "P3"
        ::$"{width} {height}"
        ::$"{depth}"
        ::pixelData
        |> String.concat newline

let canvas width height = Canvas(width, height)
let pixelAt x y (canvas: Canvas) = canvas.[x, y]
let writePixel x y color (canvas: Canvas) = canvas.[x, y] <- color; canvas
let toPpm (canvas: Canvas) = canvas.ToPpm ()
