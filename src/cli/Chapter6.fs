module Chapter6

open System.IO

open Primitives
open Matrix
open Transform
open Ray
open Sphere
open Canvas

let chapter6 width height =
    let halfWidth = width / 2
    let xShift = halfWidth - 1
    let width = 2 * halfWidth

    let halfHeight = height / 2
    let yShift = halfHeight - 1
    let height = 2 * halfHeight

    let shift = translation (float xShift) (float yShift) 0.0
    let wallDistance = 2 * min width height

    let intensity = lightYellow
    let bground = white / 20

    let canv = canvas width height
    let inline writePoint p col =
        let (Point (x, y, _)) = shift *. p
        writePixel canv (int x) ((height - 1 - int y)) col

    let rays () = seq{
        for y in [-yShift .. halfHeight] do
            for x in [-xShift .. halfWidth] do
                ray zeroPoint (vectori x y wallDistance)}

    let sDist = wallDistance / 2 |> float
    let sphere =
        let rad = sDist / 5.0
        let scale = scaling rad rad rad |* translation 0.0 0.0 sDist
        let material = material().With(color = darkRed, ambient = 0.01)
        Sphere(scale, material)

    let light = pointLight (pointi -sDist sDist (sDist / 2.0)) intensity
    let color r =
        let hit = sphere.Intersect(r) |> hit
        match hit with
        | None -> bground
        | Some intr ->
            let point = position r intr.T
            let eyev = -normalize r.Direction
            let normalv = normalAt intr.Object point
            lighting intr.Object.Material light point eyev normalv

    let writeRay r = writePoint (position r 1.0) (color r) |> ignore
    rays () |> Seq.iter writeRay

    let filename = "Ch6Sphere.ppm"
    File.WriteAllText(filename, canvasToPpm canv)
    printfn $"Written ppm to {FileInfo(filename).FullName}"
