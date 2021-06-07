open Tuple
open Projectile

let flight () =
    flight
        (environment (vector 0.0 -0.1 0.0) (vector -0.01 0.0 0.0))
        (projectile (point 0.0 1.0 0.0) (vector 1.0 1.0 0.0 |> norm))
    |> Seq.toList

let projToRow proj =
    let pos = proj.Position
    sprintf "%0.5f,%0.5f" (x pos) (y pos)

[<EntryPoint>]
let main argv =
    printfn "X,Y"
    flight ()
    |> Seq.map projToRow
    |> Seq.iter (printfn "%s")
    0
