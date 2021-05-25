module ProjectileTests

open Xunit
open Tuple
open Projectile

let assertTupleEqual a b = Assert.True(equal a b)

[<Fact>]
let projectile_hits_target () =

    let flight =
        flight
            (environment (vector 0.0 -0.1 0.0) (vector -0.01 0.0 0.0))
            (projectile (point 0.0 1.0 0.0) (vector 1.0 1.0 0.0 |> norm))
        |> Seq.toList

    let lastPos = List.last flight
    assertTupleEqual (point 10.660815 -0.579184 0.0) lastPos
    Assert.Equal(17, flight.Length - 1)
