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
        Ray(t .* r.Origin, t .* r.Direction)

// ray functions
let inline ray origin direction : Ray = Ray(origin, direction)
let inline origin (r: Ray) : Point = r.Origin
let inline direction (r: Ray) : Vector = r.Direction
let inline position (r: Ray) t = r.Position(t)


(* Material *)
[<Struct>]
type Material =  { Color: Color
                   Ambient: float
                   Diffuse: float
                   Specular: float
                   Shininess: float }

// Material functions
let material () = { Color = colori 1 1 1
                    Ambient = 0.1
                    Diffuse = 0.9
                    Specular = 0.9
                    Shininess = 200.0 }


(* Sphere *)
[<Sealed>]
type Sphere (transform: Matrix, material: Material) =
    let inverseT = transform |> inverse
    let transposeInverseT = inverseT |> transpose
    let normalAtT = transposeInverseT * inverseT

    new () = Sphere(identity (), material ())
    new (transform) = Sphere(transform, material ())
    member _.Transform = transform
    member _.Material = material

    member inline s.WithTransform(transform: Matrix) =
        Sphere(transform, s.Material)
    member inline s.Intersection time = {T = time; Object = s}

    member s.Intersect(r: Ray) : Intersections =
        let r = inverseT * r
        let sphereToRay = (origin r) - (point 0.0 0.0 0.0)
        let a = dot (direction r) (direction r)
        let b = 2.0 * (dot (direction r) sphereToRay)
        let c = (dot sphereToRay sphereToRay) - 1.0
        let disc = (b * b) - (4.0 * a * c)
        if disc < 0.0 then
            List.empty
        else
            let sqrtDisc = sqrt disc
            let t1 = (-b - sqrtDisc) / (2.0 * a)
            let t2 = (-b + sqrtDisc) / (2.0 * a)
            [ s.Intersection t1; s.Intersection t2 ]

    member _.NormalAt(point: Point) : Vector =
        point
        |* normalAtT
        |> toVectorUnchecked
        |> normalize

and [<Struct>] Intersection = {T: float; Object: Sphere}
and Intersections = Intersection list

// Sphere functions
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
let inline reflect ``in`` normal : Vector =
    ``in`` - normal * 2.0 * dot ``in`` normal


(* Point Light *)
[<Struct>]
type PointLight = {Position: Point; Intensity: Color}

// PointLight functions
let inline pointLight position intensity : PointLight =
    {Position = position; Intensity = intensity}
