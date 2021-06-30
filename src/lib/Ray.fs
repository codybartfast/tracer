module Ray

open Matrix
open Primitives

(* Ray *)
[<Struct>]
type Ray (origin: Point, direction: Vector) =
    member _.Origin = origin
    member _.Direction = direction
    member inline r.Position (time) = r.Origin + r.Direction * time
    static member inline ( * ) (t: Matrix, r: Ray) =
        Ray(t *. r.Origin, t *. r.Direction)

// ray functions
let inline ray origin direction : Ray = Ray(origin, direction)
let inline origin (r: Ray) : Point = r.Origin
let inline direction (r: Ray) : Vector = r.Direction
let inline position (r: Ray) t = r.Position(t)
