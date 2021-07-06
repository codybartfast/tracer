module Sphere

open Primitives
open Matrix
open Ray

(* Material *)
[<Struct>]
type Material =  { Color: Color
                   Ambient: float
                   Diffuse: float
                   Specular: float
                   Shininess: float }
    with
    member m.With(?color, ?ambient, ?diffuse, ?specular, ?shininess) =
        { Color = defaultArg color m.Color
          Ambient = defaultArg ambient m.Ambient
          Diffuse = defaultArg diffuse m.Diffuse
          Specular = defaultArg specular m.Specular
          Shininess = defaultArg shininess m.Shininess }

let defaultMaterial =
    { Color = colori 1 1 1
      Ambient = 0.1
      Diffuse = 0.9
      Specular = 0.9
      Shininess = 200.0 }

// material functions
let material () = defaultMaterial

(* Sphere *)
[<Sealed>]
type Sphere (?transform: Matrix, ?material: Material) =
    let transform = defaultArg transform (identity ())
    let material = defaultArg material defaultMaterial

    let inverseT = transform |> inverse
    let transposeInverseT = inverseT |> transpose
    let normalAtT = transposeInverseT * inverseT

    // new (transform) = Sphere(transform, material ())
    member _.Transform = transform
    member _.Material = material

    member inline s.With(?transform, ?material) =
        Sphere(
            defaultArg transform s.Transform,
            defaultArg material s.Material)
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
let inline intersect r (s: Sphere) = s.Intersect(r)
let inline hit xs : Option<Intersection> =
    match xs |> List.filter (fun x -> 0.0 <= x.T) with
    | x::_ -> Some x
    | _ -> None
let inline normalAt (s: Sphere) p = s.NormalAt(p)
let inline reflect ``in`` normal : Vector =
    ``in`` - normal * 2.0 * dot ``in`` normal
let equivalent (s1: Sphere) (s2: Sphere) =
    s1.Transform = s2.Transform && s1.Material = s2.Material


[<Struct>]
type PointLight = {Position: Point; Intensity: Color}
let inline pointLight position intensity : PointLight =
    {Position = position; Intensity = intensity}


let lighting material light point eyev normalv inShadow =
    let effectiveColor = material.Color * light.Intensity
    let lightv = normalize(light.Position - point)
    let ambient = effectiveColor * material.Ambient
    let lightDotNormal = dot lightv normalv
    if inShadow || lightDotNormal <= 0.0 then
        ambient
    else
        let diffuse = effectiveColor * material.Diffuse * lightDotNormal
        let reflectv = reflect -lightv normalv
        let reflectDotEye = dot reflectv eyev
        if reflectDotEye <= 0.0 then
            ambient + diffuse
        else
            let factor = reflectDotEye ** material.Shininess
            let specular = light.Intensity * material.Specular * factor
            ambient + diffuse + specular

[<Struct>]
type Computations =
    { T: float
      Object: Sphere
      Point: Point
      OverPoint: Point
      Inside: bool
      Eyev: Vector
      Normalv: Vector }

let prepareComputations (i: Intersection) r =
    let point = position r i.T
    let eyev = -r.Direction
    let initialNormal = normalAt i.Object point
    let inside = dot initialNormal eyev < 0.0
    let normalv = if inside then -initialNormal else initialNormal
    let overPoint = point + (normalv * epsilon)
    { T = i.T
      Object = i.Object
      Point = point
      OverPoint = overPoint
      Inside = inside
      Eyev = eyev
      Normalv = normalv }
