open System

open Tuple
open Projectile

let flight () =
    flight
        (environment (Tuple.vector 0.0 -0.1 0.0) (Tuple.vector -0.01 0.0 0.0))
        (projectile (Tuple.point 0.0 1.0 0.0) (Tuple.vector 1.0 1.0 0.0 |> Tuple.norm))

let posToRow pos = sprintf "%0.5f,%0.5f" (Tuple.X pos) (Tuple.Y pos)



[<EntryPoint>]
let main argv =
    flight ()
    |> Seq.map posToRow
    |> Seq.iter (printfn "%s")

    0
