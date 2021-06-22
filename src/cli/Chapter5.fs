module Chapter5

open System.IO

open Primitives
open Matrix
open Transform
open Ray
open Canvas

let chapter5 width height =
    let halfWidth = width / 2
    let xShift = halfWidth - 1
    let width = 2 * halfWidth

    let halfHeight = height / 2
    let yShift = halfHeight - 1
    let height = 2 * halfHeight

    let light = (yellow + white) * 0.5
    let shadow = blue * 0.2

    let canv = canvas width height
    let inline writePos x y col =
        writePixel (int x) (int y) col canv

    let origin = pointi xShift yShift 0
    let rays () = seq{
        for y in [-yShift .. halfHeight] do
            for x in [-xShift .. halfWidth] do
                ray origin (vectori x y 100)}

    let sphere =
        Sphere(
            scaling 45.0 45.0 45.0
            |* translation 0.0 0.0 50.0
            |* translation (float xShift) (float yShift) 0.0 )
    let colour r = if sphere.DoesIntersect(r) then shadow else light

    rays ()
        |> Seq.iter (fun r ->
            let p = (position r 1.0)
            writePos (x p) (y p) (colour r) |> ignore)

    let filename = "Ch5Sphere.ppm"
    File.WriteAllText(filename, canvasToPpm canv)
    printfn $"Written ppm to {FileInfo(filename).FullName}"
