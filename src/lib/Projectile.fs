module Projectile

open Tuple

let (++) = Tuple.add

type Projectile = { Position: Tuple; Velocity: Tuple }
type Environment = { Gravity: Tuple; Wind: Tuple }

let projectile position velocity =
    { Position = position
      Velocity = velocity }

let environment gravity wind = { Gravity = gravity; Wind = wind }

let tick (env: Environment) (proj: Projectile) =
    { Position = proj.Position ++ proj.Velocity
      Velocity = proj.Velocity ++ env.Gravity ++ env.Wind }

let rec path env =
    Seq.unfold (fun proj -> Some(proj.Position, tick env proj))

let flight env proj =
    let isAirbourne = Tuple.Y >> ((<) 0.0)
    let rec flight path = seq {
        let pos = Seq.head path
        yield pos
        if isAirbourne pos then
            yield! flight (Seq.tail path) }
    flight (path env proj)
