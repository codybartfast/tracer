module Chapter1

open Primitives
open Projectile

let chapter1 () =
    let flight () =
        flight
            (environment (vector 0.0 -0.1 0.0) (vector -0.01 0.0 0.0))
            (projectile (point 0.0 1.0 0.0) (vector 1.0 1.0 0.0 |> normalize))
        |> Seq.toList

    let posToRow proj =
        let pos = proj.Position in sprintf "%0.5f,%0.5f" (x pos) (y pos)

    printfn "X,Y"
    flight ()
    |> Seq.map posToRow
    |> Seq.iter (printfn "%s")
