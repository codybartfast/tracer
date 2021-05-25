module ProjectileTests

open Xunit
open Tuple
open Projectile

let assertTupleEqual a b = Assert.True(Tuple.equal a b)

[<Fact>]
let projectile_hits_target () =

    let flight =
        flight
            (environment
                (Tuple.vector 0.0 -0.1 0.0)
                (Tuple.vector -0.01 0.0 0.0))
            (projectile
                (Tuple.point 0.0 1.0 0.0)
                (Tuple.vector 1.0 1.0 0.0 |> Tuple.norm))
        |> Seq.toList

    let lastPos = List.last flight
    assertTupleEqual (Tuple.point 10.660815 -0.579184 0.0) lastPos
    Assert.Equal(17, flight.Length - 1)
