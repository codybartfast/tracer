module TupleTests

open Xunit
open Tuple

let assertFloatEqual a b = Assert.True(Tuple.floatEqual a b)
let assertTupleEqual a b = Assert.True(Tuple.equal a b)

[<Fact>]
let tuple_with_w1_is_a_point () =
    let x, y, z = 4.3, -4.2, 3.1
    let a = Tuple.raw x y z 1.0
    Assert.Equal(x, Tuple.X a)
    Assert.Equal(y, Tuple.Y a)
    Assert.Equal(z, Tuple.Z a)
    Assert.True(Tuple.isPoint a)
    Assert.False(Tuple.isVector a)

[<Fact>]
let tuple_with_w0_is_a_vector () =
    let x, y, z = 4.3, -4.2, 3.1
    let a = Tuple.raw x y z 0.0
    Assert.Equal(x, Tuple.X a)
    Assert.Equal(y, Tuple.Y a)
    Assert.Equal(z, Tuple.Z a)
    Assert.False(Tuple.isPoint a)
    Assert.True(Tuple.isVector a)

[<Fact>]
let point_creates_tuple_with_w1 () =
    let x, y, z = 4.3, -4.2, 3.1
    Assert.Equal(Tuple.raw x y z 1.0, Tuple.point x y z)

[<Fact>]
let vector_creates_tuple_with_w0 () =
    let x, y, z = 4.3, -4.2, 3.1
    Assert.Equal(Tuple.raw x y z 0.0, Tuple.vector x y z)

[<Fact>]
let adding_two_tuples () =
    let a1 = Tuple.raw 3.0 -2.0 5.0 1.0
    let a2 = Tuple.raw -2.0 3.0 1.0 0.0
    assertTupleEqual (Tuple.raw 1.0 1.0 6.0 1.0) (Tuple.add a1 a2)

[<Fact>]
let subtracting_two_tuples () =
    let a1 = Tuple.point 3.0 2.0 1.0
    let a2 = Tuple.point 5.0 6.0 7.0
    assertTupleEqual (Tuple.vector -2.0 -4.0 -6.0) (Tuple.sub a1 a2)

[<Fact>]
let subtracting_vector_from_point () =
    let a1 = Tuple.point 3.0 2.0 1.0
    let a2 = Tuple.vector 5.0 6.0 7.0
    assertTupleEqual (Tuple.point -2.0 -4.0 -6.0) (Tuple.sub a1 a2)

[<Fact>]
let subtracting_vector_from_zero_vecor () =
    let a1 = Tuple.vector 0.0 0.0 0.0
    let a2 = Tuple.vector 1.0 -2.0 3.0
    assertTupleEqual (Tuple.vector -1.0 2.0 -3.0) (Tuple.sub a1 a2)

[<Fact>]
let negating_a_vector () =
    let a = Tuple.raw 1.0 -2.0 3.0 -4.0
    assertTupleEqual (Tuple.raw -1.0 2.0 -3.0 4.0) (Tuple.neg a)

[<Fact>]
let multiplying_a_tuple_by_a_scalar () =
    let a = Tuple.raw 1.0 -2.0 3.0 -4.0
    assertTupleEqual (Tuple.raw 3.5 -7.0 10.5 -14.0) (Tuple.mul a 3.5)

[<Fact>]
let multiplying_a_tuple_by_a_fraction () =
    let a = Tuple.raw 1.0 -2.0 3.0 -4.0
    assertTupleEqual (Tuple.raw 0.5 -1.0 1.5 -2.0) (Tuple.mul a 0.5)

[<Fact>]
let dividing_a_tuple_by_a_scalar () =
    let a = Tuple.raw 1.0 -2.0 3.0 -4.0
    assertTupleEqual (Tuple.raw 0.5 -1.0 1.5 -2.0) (Tuple.div a 2.0)

[<Theory>]
[<InlineData(1.0, 1.0, 0.0, 0.0)>]
[<InlineData(1.0, 0.0, 1.0, 0.0)>]
[<InlineData(1.0, 0.0, 0.0, 1.0)>]
[<InlineData(3.74165739, 1.0, 2.0, 3.0)>]
[<InlineData(3.74165739, -1.0, -2.0, -3.0)>]
let computing_magnitude_of_vector (expected, x, y, z) =
    assertFloatEqual expected (Tuple.mag (Tuple.vector x y z))

[<Fact>]
let normalizing_vectors () =
    let test tup exp =
        let norm = Tuple.norm tup
        assertTupleEqual exp norm 
        assertFloatEqual 1.0 (Tuple.mag norm)
    test (Tuple.vector 4.0 0.0 0.0) (Tuple.vector 1.0 0.0 0.0)
    test (Tuple.vector 1.0 2.0 3.0) (Tuple.vector 0.26726 0.53452 0.80178)

[<Fact>]
let dot_product_of_two_tuples () =
    let a = Tuple.vector 1.0 2.0 3.0
    let b = Tuple.vector 2.0 3.0 4.0
    assertFloatEqual 20.0 (Tuple.dot a b)

[<Fact>]
let cross_product_of_two_tuples () =
    let a = Tuple.vector 1.0 2.0 3.0
    let b = Tuple.vector 2.0 3.0 4.0
    assertTupleEqual (Tuple.vector -1.0 2.0 -1.0) (Tuple.cross a b)
    assertTupleEqual (Tuple.vector 1.0 -2.0 1.0) (Tuple.cross b a)

