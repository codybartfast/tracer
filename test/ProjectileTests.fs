module ProjectileTests

open Xunit
open Tuple

type Projectile = { Position: Tuple; Velocity: Tuple }
type Environment = { Gravity: Tuple; Wind: Tuple }

let assertTupleEqual a b = Assert.True(Tuple.equal a b)

let tick (env: Environment) (proj: Projectile) =
    { Position = Tuple.add proj.Position proj.Velocity
      Velocity = List.reduce Tuple.add [ proj.Velocity; env.Gravity; env.Wind ] }

let rec path env proj =
    let pos = proj.Position
    if (Tuple.Y pos) <= 0.0 then 
        [pos]
    else 
        pos :: (path env (tick env proj))


[<Fact>]
let projectile_hits_target () =
    let proj =
        { Position = Tuple.point 0.0 1.0 0.0
          Velocity = Tuple.vector 1.0 1.0 0.0 |> Tuple.norm }
    let env =
        { Gravity = Tuple.vector 0.0 -0.1 0.0
          Wind = Tuple.vector -0.01 0.0 0.0 }
    let path = path env proj
    let lastPos = List.last path
    assertTupleEqual (Tuple.point 10.660815 -0.579184 0.0) lastPos
    Assert.Equal(17, path.Length - 1)
