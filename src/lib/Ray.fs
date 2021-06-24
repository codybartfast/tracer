module Ray

open Matrix
open Primitives

[<Struct>]
type Ray (origin: Point, direction: Vector) =
    member _.Origin = origin
    member _.Direction = direction
    member inline r.Position (time) = r.Origin + r.Direction * time
    static member inline ( * ) (t: Matrix, r: Ray) =
        Ray(t .* r.Origin, t .* r.Direction)

let inline ray origin direction : Ray = Ray(origin, direction)
let inline origin (r: Ray) : Point = r.Origin
let inline direction (r: Ray) : Vector = r.Direction
let inline position (r: Ray) t = r.Position(t)

[<Sealed>]
type Sphere (transform: Matrix) =
    let inverseM = transform |> inverse
    let transposeInverseM = transform |> inverse |> transpose
    new () = Sphere(identity ())
    member _.Transform = transform
    member inline _.WithTransform(transform: Matrix) = Sphere(transform)
    member inline s.Intersection time = {T = time; Object = s}

    member s.Intersect(r: Ray) =
        let r = inverseM * r
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

    member _.NormalAt(point: Point) =
        point
        |* inverseM
        |* transposeInverseM
        |> toVectorUnchecked
        |> normalize

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
let inline normalAt (s: Sphere) p = s.NormalAt(p)
let inline reflect i normal = i - normal * 2.0 * dot i normal