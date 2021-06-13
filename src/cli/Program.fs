open System.IO

open Canvas
open Matrix
// open Projectile
open Transform
open Tuple



[<EntryPoint>]
let main argv =
    let height = 101
    let c = canvas height height
    let writePos pos canv =
        writePixel
            (pos |> x |> int)
            (pos |> y |> int |> ((-) height))
            white
            canv

    let axle = point 51.0 51.0 0.0
    let noon = vector 0.0 48.0 0.0
    let rotate = rotation_z (-pi / 6.0)

    Seq.unfold (fun h -> Some(h, rotate |* h)) noon
    |> Seq.take 12
    |> Seq.map ((.+) axle)
    |> Seq.iter (fun h -> writePos h c |> ignore)
    
    let filename = "Ch4Clock.ppm"
    File.WriteAllText(filename, canvasToPpm c)
    printfn "Written ppm to %s" (FileInfo(filename).FullName)
    0
