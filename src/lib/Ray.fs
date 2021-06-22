module Ray

open Matrix
open Primitives

[<Struct>]
type Ray (origin: Point, direction: Vector) =
    member _.Origin = origin
    member _.Direction = direction
    member _.Position (time) = origin + direction * time
    member _.WithTransform (t: Matrix) = 
        Ray(t * origin |> toPoint, t * direction |> toVector)

let inline ray origin direction : Ray = Ray(origin, direction)
let inline origin (r: Ray) : Point = r.Origin
let inline direction (r: Ray) : Vector = r.Direction
let inline position (r: Ray) t = r.Position(t)
let inline withTransform t (r: Ray) = r.WithTransform(t)


type Sphere (transform: Matrix) =
    let inverse = inverse transform
    new () = Sphere(identity ())
    member _.Transform = transform
    member inline _.WithTransform(transform: Matrix) = Sphere(transform)
    member inline s.Intersection time = {T = time; Object = s}

    member s.Intersect(r) =
        let r = withTransform inverse r
        let sphereToRay = (origin r) - (point 0.0 0.0 0.0)
        let a = dot (direction r) (direction r)
        let b = 2.0 * (dot (direction r) sphereToRay)
        let c = (dot sphereToRay sphereToRay) - 1.0
        let disc = (b * b) - (4.0 * a * c)
        if disc < 0.0 then
            Array.empty
        else
            let sqrtDisc = sqrt disc
            let t1 = (-b - sqrtDisc) / (2.0 * a)
            let t2 = (-b + sqrtDisc) / (2.0 * a)
            [| s.Intersection t1; s.Intersection t2 |]

    member s.DoesIntersect(r) = s.Intersect(r) |> Array.isEmpty |> not

and [<Struct>] Intersection = {T: float; Object: Sphere}
and Intersections = Intersection list

let inline sphere () = Sphere ()
let inline intersection t (s: Sphere) = s.Intersection(t)
let inline intersections (xs: Intersection list) : Intersections = 
    xs |> List.sortBy (fun x -> x.T)
let inline intersect (s: Sphere) r = s.Intersect(r)
let inline hit xs : Option<Intersection> =
    match xs |> List.filter (fun x -> 0.0 <= x.T) with
    | x::_ -> Some x
    | _ -> None
