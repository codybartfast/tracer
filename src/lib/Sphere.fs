module Sphere

open Primitives
open Matrix
open Ray
open Shapes


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

    override s.LocalNormalAt (point: Point) = point - zeroPoint

// Sphere functions
let inline sphere () = Sphere ()
let equivalent (s1: Sphere) (s2: Sphere) =
    s1.Transform = s2.Transform && s1.Material = s2.Material
