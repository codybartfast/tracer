module Chapter2

open System.IO

open Canvas
open Primitives
open Projectile

let chapter2 () =
    let start = point 0.0 1.0 0.0
    let velocity =  (vector 1.0 1.8 0.0 |> normalize) * 11.25
    let p = projectile start velocity
    let gravity = vector 0.0 -0.1 0.0
    let wind = vector -0.01 0.0 0.0
    let e = environment gravity wind
    let height = 500
    let c = canvas 900 height

    let path = flight e p |> Seq.map position |> List.ofSeq
    let minX = path |> List.map x |> List.reduce min
    let maxX = path |> List.map x |> List.reduce max
    let minY = path |> List.map y |> List.reduce min
    let maxY = path |> List.map y |> List.reduce max
    printfn $" x range: {minX} - {maxX}"
    printfn $" y range: {minY} - {maxY}"

    let writePos canv pos =
        writePixel
            canv
            (pos |> x |> int)
            (pos |> y |> int |> ((-) height))
            (color 1.0 0.0 0.0)
        canv

    let ppm =
        path
        |> List.fold writePos c
        |> canvasToPpm
    let filename = "Ch2Projectile.ppm"
    File.WriteAllText(filename, ppm)
    printfn "Written ppm to %s" (FileInfo(filename).FullName)
