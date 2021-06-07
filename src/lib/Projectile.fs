module Projectile

open Tuple

type Projectile = { Position: Tuple; Velocity: Tuple }
type Environment = { Gravity: Tuple; Wind: Tuple }

let projectile position velocity =
    { Position = position
      Velocity = velocity }
let position projectile = projectile.Position

let environment gravity wind = { Gravity = gravity; Wind = wind }

let tick env proj =
    { Position = proj.Position .+ proj.Velocity
      Velocity = proj.Velocity .+ env.Gravity .+ env.Wind }

let rec path env =
    Seq.unfold (fun proj -> Some(proj, tick env proj))

let flight env =
    path env
    >> Seq.takeWhile (fun proj -> proj.Position |> y |> ((<=) 0.0))
