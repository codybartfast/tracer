module Shapes

open System
open Primitives
open Matrix
open Ray
open ShapeBase
open Transformations


(* Sphere *)
[<Sealed>]
type Sphere (?transform: Matrix, ?material: Material) =
    inherit Shape(transform, material)

    member inline s.With(?transform, ?material) =
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
