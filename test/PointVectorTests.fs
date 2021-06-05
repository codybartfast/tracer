module PointVectorTests

open Xunit
open Tuple

let assertValEqual a b = Assert.True(valEqual a b)
let assertTupleEqual a b = Assert.True(equal a b)

[<Fact>]
let point_creates_a_point () =
    let x, y, z = 4.3, -4.2, 3.1
    let a = point x y z
    Assert.Equal(x, X a)
    Assert.Equal(y, Y a)
    Assert.Equal(z, Z a)
    Assert.True(isPoint a)
    Assert.False(isVector a)

[<Fact>]
let vector_create_a_vector () =
    let x, y, z = 4.3, -4.2, 3.1
    let a = vector x y z
    Assert.Equal(x, X a)
    Assert.Equal(y, Y a)
    Assert.Equal(z, Z a)
    Assert.False(isPoint a)
    Assert.True(isVector a)

// [<Fact>]
// let point_creates_tuple_with_w1 () =
//     let x, y, z = 4.3, -4.2, 3.1
//     Assert.Equal(rawTuple x y z 1.0, point x y z)

// [<Fact>]
// let vector_creates_tuple_with_w0 () =
//     let x, y, z = 4.3, -4.2, 3.1
//     Assert.Equal(rawTuple x y z 0.0, vector x y z)

[<Fact>]
let adding_two_tuples () =
    let a1 = point 3.0 -2.0 5.0
    let a2 = vector -2.0 3.0 1.0
    assertTupleEqual (point 1.0 1.0 6.0) (add a1 a2)
    assertTupleEqual (point 1.0 1.0 6.0) (a1 .+ a2)

[<Fact>]
let subtracting_two_tuples () =
    let a1 = point 3.0 2.0 1.0
    let a2 = point 5.0 6.0 7.0
    assertTupleEqual (vector -2.0 -4.0 -6.0) (sub a1 a2)
    assertTupleEqual (vector -2.0 -4.0 -6.0) (a1 .- a2)

[<Fact>]
let subtracting_vector_from_point () =
    let a1 = point 3.0 2.0 1.0
    let a2 = vector 5.0 6.0 7.0
    assertTupleEqual (point -2.0 -4.0 -6.0) (sub a1 a2)
    assertTupleEqual (point -2.0 -4.0 -6.0) (a1 .- a2)

[<Fact>]
let subtracting_vector_from_zero_vecor () =
    let a1 = vector 0.0 0.0 0.0
    let a2 = vector 1.0 -2.0 3.0
    assertTupleEqual (vector -1.0 2.0 -3.0) (sub a1 a2)
    assertTupleEqual (vector -1.0 2.0 -3.0) (a1 .- a2)

[<Fact>]
let negating_a_vector () =
    let a = vector 1.0 -2.0 3.0
    assertTupleEqual (vector -1.0 2.0 -3.0) (neg a)

[<Fact>]
let multiplying_a_tuple_by_a_scalar () =
    let a = vector 1.0 -2.0 3.0
    assertTupleEqual (vector 3.5 -7.0 10.5) (mul a 3.5)
    assertTupleEqual (vector 3.5 -7.0 10.5) (a .* 3.5)

[<Fact>]
let multiplying_a_tuple_by_a_fraction () =
    let a = vector 1.0 -2.0 3.0
    assertTupleEqual (vector 0.5 -1.0 1.5) (mul a 0.5)
    assertTupleEqual (vector 0.5 -1.0 1.5) (a .* 0.5)

[<Fact>]
let dividing_a_tuple_by_a_scalar () =
    let a = vector 1.0 -2.0 3.0
    assertTupleEqual (vector 0.5 -1.0 1.5) (div a 2.0)
    assertTupleEqual (vector 0.5 -1.0 1.5) (a ./ 2.0)

[<Theory>]
[<InlineData(1.0, 1.0, 0.0, 0.0)>]
[<InlineData(1.0, 0.0, 1.0, 0.0)>]
[<InlineData(1.0, 0.0, 0.0, 1.0)>]
[<InlineData(3.74165739, 1.0, 2.0, 3.0)>]
[<InlineData(3.74165739, -1.0, -2.0, -3.0)>]
let computing_magnitude_of_vector (expected, x, y, z) =
    assertValEqual expected (mag (vector x y z))

[<Fact>]
let normalizing_vectors () =
    let test tup exp =
        let norm = norm tup
        assertTupleEqual exp norm
        assertValEqual 1.0 (mag norm)
    test (vector 4.0 0.0 0.0) (vector 1.0 0.0 0.0)
    test (vector 1.0 2.0 3.0) (vector 0.26726 0.53452 0.80178)

[<Fact>]
let dot_product_of_two_tuples () =
    let a = vector 1.0 2.0 3.0
    let b = vector 2.0 3.0 4.0
    assertValEqual 20.0 (dot a b)

[<Fact>]
let cross_product_of_two_tuples () =
    let a = vector 1.0 2.0 3.0
    let b = vector 2.0 3.0 4.0
    assertTupleEqual (vector -1.0 2.0 -1.0) (cross a b)
    assertTupleEqual (vector 1.0 -2.0 1.0) (cross b a)
