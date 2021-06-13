module TransfromTests

open System
open Xunit

open Matrix
open Transform
open Tuple

type Assert = XUnitExtensions.TracerAssert

// let matrixOfLists  (lists: 'a list list) =
//         lists |> (List.map List.toArray) |> List.toArray |> matrixOfRows
// let matrixOfInts = matrixOfLists >> (Array2D.map float)
// let round = Array2D.map (fun (n: float) -> Math.Round(n, 5))


[<Fact>]
let multiplying_by_a_translation_matrix () =
    let transform = translation 5.0 -3.0 2.0
    let p = point -3.0 4.0 5.0
    Assert.TupleEqual(point 2.0 1.0 7.0, transform |* p)

[<Fact>]
let multiplying_by_the_inverse_of_a_translation_matrix () =
    let transform = translation 5.0 -3.0 2.0
    let inv = inverse transform
    let p = point -3.0 4.0 5.0
    Assert.TupleEqual(point -8.0 7.0 3.0, inv |* p)

[<Fact>]
let translation_does_not_affect_vectors () =
    let transform = translation 5.0 -3.0 2.0
    let v = vector -3.0 4.0 5.0
    Assert.TupleEqual(v, transform |* v)