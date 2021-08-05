module Shapes

open System

open Primitives
open Matrix
open Ray
open ShapeBase


(* Sphere *)
[<Sealed>]
type Sphere (?transform: Matrix, ?material: Material) =
    inherit Shape(transform, material)

    member s.With(?transform, ?material) =
        Sphere(
            defaultArg transform s.Transform,
            defaultArg material s.Material)

    override s.LocalIntersect(r: Ray) : Intersections =
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

    override _.LocalNormalAt (point: Point) = point - zeroPoint


// Sphere functions
let inline sphere () = Sphere ()
let equivalent (s1: Shape) (s2: Shape) =
    s1.Transform = s2.Transform && s1.Material = s2.Material
let glass = material.With(transparency = 1.0, refractiveIndex = 1.5)


(* Plane *)
[<Sealed>]
type Plane (?transform: Matrix, ?material: Material) =
    inherit Shape(transform, material)

    let normalAt = vectori 0 1 0

    member inline p.With(?transform, ?material) =
        Plane(
            defaultArg transform p.Transform,
            defaultArg material p.Material)

    override p.LocalIntersect(ray: Ray) : Intersections =
        if valEqual (y ray.Direction) 0.0 then
            List.empty
        else
            let t = -ray.Origin.Y / ray.Direction.Y
            [intersection t p]

    override _.LocalNormalAt (_: Point) = normalAt

let inline plane () = Plane ()


(* Cube *)
[<Sealed>]
type Cube (?transform: Matrix, ?material: Material) =
    inherit Shape(transform, material)

    let checkAxis origin direction =
        let tminNumerator = -1.0 - origin
        let tmaxNumerator = 1.0 - origin
        let tmin, tmax =
            if abs(direction) >= epsilon then
                tminNumerator / direction, tmaxNumerator / direction
            else
                tminNumerator * Double.PositiveInfinity,
                    tmaxNumerator * Double.PositiveInfinity
        if tmin <= tmax then tmin, tmax else tmax, tmin


    override c.LocalIntersect(ray: Ray) : Intersections =
        let xtmin, xtmax = checkAxis ray.Origin.X ray.Direction.X
        let ytmin, ytmax = checkAxis ray.Origin.Y ray.Direction.Y
        let ztmin, ztmax = checkAxis ray.Origin.Z ray.Direction.Z
        let tmin = List.max [xtmin; ytmin; ztmin]
        let tmax = List.min [xtmax; ytmax; ztmax]
        if tmax <= 0.0 || tmax < tmin then
            []
        else
            [intersection tmin c; intersection tmax c]

    override _.LocalNormalAt (point: Point) =
        let absolutes = [point.X; point.Y; point.Z] |> List.map abs
        let maxC =  absolutes |> List.max
        match List.findIndex ((=) maxC) absolutes with
        | 0 -> vector point.X 0.0 0.0
        | 1 -> vector 0.0 point.Y 0.0
        | _ -> vector 0.0 0.0 point.Z


[<Sealed>]
type Cylinder (?transform: Matrix, ?material: Material) =
    inherit Shape(transform, material)

    let sqr n = n * n

    override cyl.LocalIntersect(ray: Ray) : Intersections =
        let a =  sqr ray.Direction.X  + sqr ray.Direction.Z
        if valEqual a 0.0 then
            []
        else
            let b = (2.0 * ray.Origin.X * ray.Direction.X) +
                        (2.0 *  ray.Origin.Z * ray.Direction.Z)
            let c = (sqr ray.Origin.X) + (sqr ray.Origin.Z) - 1.0
            let disc = (sqr b) - 4.0 * a * c
            if disc < 0.0 then 
                [] 
            else
                let t0 = (-b - (sqrt disc)) / (2.0 * a)
                let t1 = (-b + (sqrt disc)) / (2.0 * a)
                [intersection t0 cyl; intersection t1 cyl]


    override _.LocalNormalAt (point: Point) =
        failwith "so"