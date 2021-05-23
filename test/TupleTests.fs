module Tests

open Xunit
open Tuple

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

