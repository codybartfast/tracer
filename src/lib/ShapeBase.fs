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
                   Shininess: float
                   Reflective: float
                   Transparency: float
                   RefractiveIndex: float }
    with
    member m.With(?color,
                  ?pattern,
                  ?ambient,
                  ?diffuse,
                  ?specular,
                  ?shininess,
                  ?reflective,
                  ?transparency,
                  ?refractiveIndex) =
        let pattern =
            match color, pattern with
            | _, Some pattern -> pattern
            | Some color, _ -> SolidPattern(color) :> Pattern
            | _ -> m.Pattern
        { Pattern = pattern
          Ambient = defaultArg ambient m.Ambient
          Diffuse = defaultArg diffuse m.Diffuse
          Specular = defaultArg specular m.Specular
          Shininess = defaultArg shininess m.Shininess
          Reflective = defaultArg reflective m.Reflective
          Transparency = defaultArg transparency m.Transparency
          RefractiveIndex = defaultArg refractiveIndex m.RefractiveIndex }

let defaultMaterial =
    { Pattern = SolidPattern(white)
      Ambient = 0.1
      Diffuse = 0.9
      Specular = 0.9
      Shininess = 200.0
      Reflective = 0.0
      Transparency = 0.0
      RefractiveIndex = 1.0 }

let material = defaultMaterial


(* Shape *)
[<AbstractClass>]
type Shape (transform: Option<Matrix>, material: Option<Material>) =
    let transform = defaultArg transform (identity ())
    let material = defaultArg material defaultMaterial

    let inverseT = transform |> inverse
    let transposeInverseT = inverseT |> transpose

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


(* Intersection *)
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


(* Point Light *)
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


(* Computation *)
[<Struct>]
type Computations =
    { T: float
      Object: Shape
      Point: Point
      OverPoint: Point
      UnderPoint: Point
      Inside: bool
      Eyev: Vector
      Normalv: Vector
      Reflectv: Vector
      N1: float
      N2: float }

let n1n2 (hit: Intersection) (xs: Intersections) =
    let prevContainers, containers =
        xs
        |> List.takeWhile (fun x -> x.T <= hit.T)
        |> List.fold
            (fun (_, containers) x ->
                let obj = x.Object
                match List.contains obj containers with
                | true -> containers, List.filter ((<>) obj) containers
                | _ -> containers, obj::containers)
            ([], [])
    let ri (containers: Shape list) =
        match containers with
        | [] -> material.RefractiveIndex
        | c::_ -> c.Material.RefractiveIndex
    ri prevContainers, ri containers

let prepareComputations (i: Intersection) r (xs: Intersections) =
    let point = position r i.T
    let eyev = -r.Direction
    let initialNormal = normalAt i.Object point
    let inside = dot initialNormal eyev < 0.0
    let normalv = if inside then -initialNormal else initialNormal
    let n1, n2 = n1n2 i xs
    { T = i.T
      Object = i.Object
      Point = point
      OverPoint = point + (normalv * epsilon)
      UnderPoint = point - (normalv * epsilon)
      Inside = inside
      Eyev = eyev
      Normalv = normalv
      Reflectv = reflect r.Direction normalv
      N1 = n1
      N2 = n2 }

let schlick comps =
    let schlick cos =
        let r0 = ((comps.N1 - comps.N2) / (comps.N1 + comps.N2)) ** 2.0
        r0 + (1.0 - r0) * (1.0 - cos) ** 5.0
    let cos = dot comps.Eyev comps.Normalv
    if comps.N1 > comps.N2 then
        let n = comps.N1 / comps.N2
        let sin2T = n * n * (1.0 - cos * cos)
        if sin2T > 1.0 then
            1.0
        else
            schlick (1.0 - sin2T |> sqrt)
    else
        schlick cos

