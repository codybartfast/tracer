module CanvasTests

open System
open Xunit

open Primitives
open Canvas

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``creating a canvas`` () =
    let c = canvas 10 20
    Assert.Equal(10, c.Width)
    Assert.Equal(20, c.Height)
    Assert.All(c.Pixels, fun px -> Assert.Equal(color 0.0 0.0 0.0, px))

[<Fact>]
let ``writing pixels to a canvas`` () =
    let c = canvas 10 20
    let red = color 1.0 0.0 0.0
    c.[2, 3] <- red
    Assert.Equal(red, c.[2, 3])

let lines first last (str: string) =
    let newline = "\n"
    str.Split(newline).[(first - 1)..(last - 1)]
    |> String.concat newline

[<Fact>]
let ``constructing the ppm header`` () =
    let expected = "P3\n5 3\n255"
    let actual = (canvas 5 3).ToPpm() |> lines 1 3
    Assert.Equal(expected, actual)

[<Fact>]
let ``constructing the ppm pixel data`` () =
    let expected =
        "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n"
        + "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n"
        + "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
    let actual =
        let canv = canvas 5 3
        writePixel canv 0 0 (color 1.5 0.0 0.0)
        writePixel canv 2 1 (color 0.0 0.5 0.0)
        writePixel canv 4 2 (color -0.5 0.0 1.0)
        canv |> canvasToPpm |> lines 4 6
    Assert.Equal(expected, actual)

[<Fact>]
let ``splitting long lines in ppm files`` () =
    let expected =
        "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n"
        + "153 255 204 153 255 204 153 255 204 153 255 204 153\n"
        + "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n"
        + "153 255 204 153 255 204 153 255 204 153 255 204 153"
    let actual =
        Canvas(10, 2, color 1.0 0.8 0.6)
        |> canvasToPpm
        |> lines 4 7
    Assert.Equal(expected, actual)

[<Fact>]
let ``ppm files are terminated with a newline characer`` () =
    let lastChar =
        (canvas 5 3 |> canvasToPpm).ToCharArray()
        |> Array.last
    Assert.Equal('\n', lastChar)
