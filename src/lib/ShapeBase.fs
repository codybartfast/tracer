module ShapeBase

open Patterns
open Primitives
open Matrix
open Ray

(* Material *)
[<Struct>]
type Material =  { Pattern: Pattern
                   Ambient: float
                   Diffuse: float
                   Specular: float
                   Shininess: float }
    with
    member m.With(?color, ?pattern, ?ambient, ?diffuse, ?specular, ?shininess) =
        let pattern =
            match color, pattern with
            | _, Some pattern -> pattern
            | Some color, _ -> SolidPattern(color) :> Pattern
            | _ -> m.Pattern
        { Pattern = pattern
          Ambient = defaultArg ambient m.Ambient
          Diffuse = defaultArg diffuse m.Diffuse
          Specular = defaultArg specular m.Specular
          Shininess = defaultArg shininess m.Shininess }

let defaultMaterial =
    { Pattern = SolidPattern(white)
      Ambient = 0.1
      Diffuse = 0.9
      Specular = 0.9
      Shininess = 200.0 }

// material functions
let material () = defaultMaterial


[<AbstractClass>]
type Shape (transform: Option<Matrix>, material: Option<Material>) =
    let transform = defaultArg transform (identity ())
    let material = defaultArg material defaultMaterial

    let inverseT = transform |> inverse
    let transposeInverseT = inverseT |> transpose
    // let normalAtT = transposeInverseT * inverseT

    member _.Transform = transform
    member _.Material = material

    abstract member LocalIntersect : Ray -> Intersections
    abstract member LocalNormalAt : Point -> Vector

    member s.Intersect (ray: Ray) : Intersections =
        ray
        |* inverseT
        |> s.LocalIntersect

    member s.NormalAt(point: Point) : Vector =
        point
        |*. inverseT
        |> s.LocalNormalAt
        |* transposeInverseT
        |> toVectorUnchecked
        |> normalize

    member _.ColorAt(point: Point) : Color =
        point
        |*. inverseT
        |> material.Pattern.ColorAt

    member inline s.Intersection time = {T = time; Object = s}

and Shapes = Shape list
and [<Struct>] Intersection = {T: float; Object: Shape}
and Intersections = Intersection list

let inline intersection t (s: Shape) = s.Intersection(t)
let inline intersections (xs: Intersection list) : Intersections =
    xs |> List.sortBy (fun x -> x.T)
let inline intersect r (s: Shape) = s.Intersect(r)
let inline hit xs : Option<Intersection> =
    match xs |> List.filter (fun x -> 0.0 <= x.T) with
    | x::_ -> Some x
    | _ -> None
let inline normalAt (s: Shape) p = s.NormalAt(p)
let inline reflect ``in`` normal : Vector =
    ``in`` - normal * 2.0 * dot ``in`` normal


[<Struct>]
type PointLight = {Position: Point; Intensity: Color}
let inline pointLight position intensity : PointLight =
    {Position = position; Intensity = intensity}


let lighting material (object: Shape) light point eyev normalv inShadow =
    let effectiveColor = object.ColorAt point * light.Intensity
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
      Object: Shape
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
