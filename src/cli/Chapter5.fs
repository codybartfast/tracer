module Chapter5

open System.IO

open Primitives
open Transform
open Ray
open Sphere
open Canvas

let chapter5 width height =
    let halfWidth = width / 2
    let xShift = halfWidth - 1
    let width = 2 * halfWidth

    let halfHeight = height / 2
    let yShift = halfHeight - 1
    let height = 2 * halfHeight

    let shift = translation (float xShift) (float yShift) 0.0
    let wallDistance = 2 * min width height

    let light = (black + white) * 0.5
    let shadow = red * 0.2

    let canv = canvas width height
    let inline writePoint p col =
        let (Point (x, y, z)) = shift *. p
        writePixel canv (int x) (int y) col

    let origin = pointi 0 0 0
    // rays that hit individual 'pixels' on the wall after 1 second
    let rays () = seq{
        for y in [-yShift .. halfHeight] do
            for x in [-xShift .. halfWidth] do
                ray origin (vectori x y wallDistance)}

    let sphere =
        let rad = (min width height) / 5 |> float
        Sphere(
            scaling rad rad rad
            * translation 0.0 0.0 (wallDistance / 2 |> float) )
    let color r =
        match sphere.Intersect(r) |> hit with None -> light | _ -> shadow

    let writeRay r = writePoint (position r 1.0) (color r) |> ignore
    rays () |> Seq.iter writeRay

    let filename = "Ch5Sphere.ppm"
    File.WriteAllText(filename, canvasToPpm canv)
    printfn $"Written ppm to {FileInfo(filename).FullName}"
