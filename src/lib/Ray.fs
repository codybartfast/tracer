module Ray

open Tuple

let ray origin direction = 
    if isPoint origin && isVector direction then
        (struct(origin, direction))
    else
        failwith "Ray: expected a point and a vector"

let origin (struct(o, d)) = o
let direction (struct(o, d)) = d

let position ray t = origin ray .+ direction ray .* t

type Sphere () =
    
    member _.Intersects(ray) =
        let sphereToRay = (origin ray) .- (point 0.0 0.0 0.0)
        let a = dot (direction ray) (direction ray)
        let b = 2.0 * (dot (direction ray) sphereToRay)
        let c = (dot sphereToRay sphereToRay) - 1.0
        let discriminant = (b * b) - (4.0 * a * c)
        if discriminant < 0.0 then
            Array.empty
        else
            let sqrtDisc = sqrt discriminant
            let t1 = (-b - sqrtDisc) / (2.0 * a)
            let t2 = (-b + sqrtDisc) / (2.0 * a)
            [| t1; t2 |]

let intersects (s: Sphere) r = s.Intersects(r)