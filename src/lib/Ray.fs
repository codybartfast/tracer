module Ray

open Primitives

type Ray = (struct(Point * Vector))

let ray (origin: Point) (direction: Vector) : Ray = (struct(origin, direction))
let origin (struct(o, d)) : Point = o
let direction (struct(o, d)) : Vector = d
let position ray t = origin ray + direction ray * t

type Sphere () =
    member _.Intersects(ray) =
        let sphereToRay = (origin ray) - (point 0.0 0.0 0.0)
        let a = dot (direction ray) (direction ray)
        let b = 2.0 * (dot (direction ray) sphereToRay)
        let c = (dot sphereToRay sphereToRay) - 1.0
        let disc = (b * b) - (4.0 * a * c)
        if disc < 0.0 then
            Array.empty
        else
            let sqrtDisc = sqrt disc
            let t1 = (-b - sqrtDisc) / (2.0 * a)
            let t2 = (-b + sqrtDisc) / (2.0 * a)
            [| t1; t2 |]

let intersects (s: Sphere) r = s.Intersects(r)
