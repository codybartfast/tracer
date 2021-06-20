module ProjectileTests

open Xunit
open Primitives
open Projectile

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``projectile hits target`` () =

    let flight =
        flight
            (environment (vector 0.0 -0.1 0.0) (vector -0.01 0.0 0.0))
            (projectile (point 0.0 1.0 0.0) (vector 1.0 1.0 0.0 |> norm))
        |> Seq.map (fun proj -> proj.Position)
        |> List.ofSeq

    let lastPos = List.last flight
    Assert.PointEqual(point 10.113708 0.313708 0.0, lastPos)
    Assert.Equal(16, flight.Length - 1)
