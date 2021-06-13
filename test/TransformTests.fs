module TransfromTests

open System
open Xunit

open Matrix
open Transform
open Tuple

type Assert = XUnitExtensions.TracerAssert
let assertTupEq e a = Assert.TupleEqual (e, a)

let pointi x y z = point (float x) (float y) (float z)
let vectori x y z = vector (float x) (float y) (float z)
let hr2 = (Math.Sqrt 2.0) / 2.0

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

[<Fact>]
let a_scaling_matrix_applied_to_a_point () =
    let transform = scaling 2.0 3.0 4.0
    let p = point -4.0 6.0 8.0
    Assert.TupleEqual(point -8.0 18.0 32.0, transform |* p)

[<Fact>]
let a_scaling_matrix_applied_to_a_vector () =
    let transform = scaling 2.0 3.0 4.0
    let v = vector -4.0 6.0 8.0
    Assert.TupleEqual(vector -8.0 18.0 32.0, transform |* v)

[<Fact>]
let multiplying_by_the_inverse_of_a_scaling_matrix () =
    let transform = scaling 2.0 3.0 4.0
    let inv = inverse transform
    let v = vector -4.0 6.0 8.0
    Assert.TupleEqual(vector -2.0 2.0 2.0, inv |* v)

[<Fact>]
let reflection_is_scaling_by_a_negative_value () =
    let transform = scaling -1.0 1.0 1.0
    let p = point 2.0 3.0 4.0
    Assert.TupleEqual(point -2.0 3.0 4.0, transform |* p)

[<Fact>]
let rotating_a_point_around_the_x_axis () =
    let p = pointi 0 1 0
    let half_quater = rotation_x (pi / 4.0)
    let full_quater = rotation_x (pi / 2.0)
    Assert.TupleEqual(point 0.0 hr2 hr2, half_quater |* p)
    Assert.TupleEqual(pointi 0 0 1, full_quater |* p)

[<Fact>]
let the_inverse_of_an_x_rotation_rotates_in_the_opposite_direction () =
    let p = pointi 0 1 0
    let half_quater = rotation_x (pi / 4.0)
    let inv = inverse half_quater
    Assert.TupleEqual(point 0.0 hr2 -hr2, inv |* p)

[<Fact>]
let rotating_a_point_around_the_y_axis () =
    let p = pointi 0  0 1
    let half_quater = rotation_y (pi / 4.0)
    let full_quater = rotation_y (pi / 2.0)
    Assert.TupleEqual(point hr2 0.0 hr2, half_quater |* p)
    Assert.TupleEqual(pointi 1 0 0, full_quater |* p)

[<Fact>]
let rotating_a_point_around_the_z_axis () =
    let p = pointi 0  1 0
    let half_quater = rotation_z (pi / 4.0)
    let full_quater = rotation_z (pi / 2.0)
    Assert.TupleEqual(point -hr2 hr2 0.0, half_quater |* p)
    Assert.TupleEqual(pointi -1 0 0, full_quater |* p)

[<Fact>]
let a_shearing_transformation_moves_x_in_proportion_to_y () =
    let transform = shearing 1.0 0.0 0.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.TupleEqual(pointi 5 3 4, transform |* p)

[<Fact>]
let a_shearing_transformation_moves_x_in_proportion_to_z () =
    let transform = shearing 0.0 1.0 0.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.TupleEqual(pointi 6 3 4, transform |* p)

[<Fact>]
let a_shearing_transformation_moves_y_in_proportion_to_x () =
    let transform = shearing 0.0 0.0 1.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.TupleEqual(pointi 2 5 4, transform |* p)

[<Fact>]
let a_shearing_transformation_moves_y_in_proportion_to_z () =
    let transform = shearing 0.0 0.0 0.0 1.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.TupleEqual(pointi 2 7 4, transform |* p)

[<Fact>]
let a_shearing_transformation_moves_z_in_proportion_to_x () =
    let transform = shearing 0.0 0.0 0.0 0.0 1.0 0.0
    let p = pointi 2 3 4
    Assert.TupleEqual(pointi 2 3 6, transform |* p)

[<Fact>]
let a_shearing_transformation_moves_z_in_proportion_to_y () =
    let transform = shearing 0.0 0.0 0.0 0.0 0.0 1.0
    let p = pointi 2 3 4
    Assert.TupleEqual(pointi 2 3 7, transform |* p)

[<Fact>]
let individual_transformations_are_applied_in_sequence () =
    let p = pointi 1 0 1
    let A = rotation_x (pi / 2.0)
    let B = scaling 5.0 5.0 5.0
    let C = translation 10.0 5.0 7.0
    let p2 = A |* p
    Assert.TupleEqual(pointi 1 -1 0, p2)
    let p3 = B |* p2
    Assert.TupleEqual(pointi 5 -5 0, p3)
    let p4 = C |* p3
    Assert.TupleEqual(pointi 15 0 7, p4)

[<Fact>]
let chained_transformations_must_be_applied_in_reverse_order () =
    let p = pointi 1 0 1
    let A = rotation_x (pi / 2.0)
    let B = scaling 5.0 5.0 5.0
    let C = translation 10.0 5.0 7.0
    let T = C |*| B |*| A
    Assert.TupleEqual(pointi 15 0 7, T |* p)

[<Fact>]
let fluent_transformations () =
    let T = 
        identity ()
        |*|> rotation_x (pi / 2.0)
        |*|> scaling 5.0 5.0 5.0
        |*|> translation 10.0 5.0 7.0
    assertTupEq (pointi 15 0 7) (T |* pointi 1 0 1)

[<Fact>]
let fluent_transformations2 () =
    pointi 1 0 1
    |*> rotation_x (pi / 2.0)
    |*> scaling 5.0 5.0 5.0
    |*> translation 10.0 5.0 7.0
    |> assertTupEq (pointi 15 0 7)
