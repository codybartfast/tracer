module TestShape

open Matrix
open Primitives
open Ray
open ShapeBase

[<Sealed>]
type TestShape (?transform: Matrix, ?material: Material) =
    inherit Shape(transform, material)

    let mutable savedRay : Option<Ray> = None

    override s.LocalIntersect ray =
        savedRay <- Some ray
        []

    override s.LocalNormalAt point =
        point - zeroPoint

    member _.SavedRay =
        match savedRay with
        | None -> failwith "SavedRay not set"
        | Some r -> r


let testShape () = TestShape()