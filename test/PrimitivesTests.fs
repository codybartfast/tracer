module PointVectorTests

open Xunit
open Primitives

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``point creates a Point`` () =
    let ex, ey, ez = 4.3, -4.2, 3.1
    let a = point ex ey ez
    Assert.Equal(ex, x a)
    Assert.Equal(ey, y a)
    Assert.Equal(ez, z a)

[<Fact>]
let ``vector creates a Vector`` () =
    let ex, ey, ez = 4.3, -4.2, 3.1
    let a = vector ex ey ez
    Assert.Equal(ex, x a)
    Assert.Equal(ey, y a)
    Assert.Equal(ez, z a)

// [<Fact>]
// let point_creates_tuple_with_w1 () =
//     let x, y, z = 4.3, -4.2, 3.1
//     Assert.Equal(rawTuple x y z 1.0, point x y z)

// [<Fact>]
// let vector_creates_tuple_with_w0 () =
//     let x, y, z = 4.3, -4.2, 3.1
//     Assert.Equal(rawTuple x y z 0.0, vector x y z)

[<Fact>]
let ``adding two tuples`` () =
    let a1 = point 3.0 -2.0 5.0
    let a2 = vector -2.0 3.0 1.0
    Assert.Equal(point 1.0 1.0 6.0, a1 + a2)

[<Fact>]
let ``subtracting two tuples`` () =
    let a1 = point 3.0 2.0 1.0
    let a2 = point 5.0 6.0 7.0
    Assert.Equal(vector -2.0 -4.0 -6.0, a1 - a2)

[<Fact>]
let ``subtracting vector from point`` () =
    let a1 = point 3.0 2.0 1.0
    let a2 = vector 5.0 6.0 7.0
    Assert.Equal(point -2.0 -4.0 -6.0, a1 - a2)

[<Fact>]
let ``subtracting vector from zero vector`` () =
    let a1 = vector 0.0 0.0 0.0
    let a2 = vector 1.0 -2.0 3.0
    Assert.Equal(vector -1.0 2.0 -3.0, a1 - a2)

[<Fact>]
let ``negating a vector`` () =
    let a = vector 1.0 -2.0 3.0
    Assert.Equal(vector -1.0 2.0 -3.0, -a)

[<Fact>]
let ``multiplying a tuple by a scalar`` () =
    let a = vector 1.0 -2.0 3.0
    Assert.Equal(vector 3.5 -7.0 10.5, a * 3.5)

[<Fact>]
let ``multiplying a tuple by a fraction`` () =
    let a = vector 1.0 -2.0 3.0
    Assert.Equal(vector 0.5 -1.0 1.5, a * 0.5)

[<Fact>]
let ``dividing a tuple by a scalar`` () =
    let a = vector 1.0 -2.0 3.0
    Assert.Equal(vector 0.5 -1.0 1.5, a / 2.0)

[<Theory>]
[<InlineData(1.0, 1.0, 0.0, 0.0)>]
[<InlineData(1.0, 0.0, 1.0, 0.0)>]
[<InlineData(1.0, 0.0, 0.0, 1.0)>]
[<InlineData(3.74165739, 1.0, 2.0, 3.0)>]
[<InlineData(3.74165739, -1.0, -2.0, -3.0)>]
let ``computing magnitude of a vector`` (expected, x, y, z) =
    Assert.ValEqual(expected, mag (vector x y z))

[<Fact>]
let ``normalizing vectors`` () =
    let test tup exp =
        let norm = norm tup
        Assert.Equal(exp, norm)
        Assert.ValEqual(1.0, mag norm)
    test (vector 4.0 0.0 0.0) (vector 1.0 0.0 0.0)
    test (vector 1.0 2.0 3.0) (vector 0.26726 0.53452 0.80178)

[<Fact>]
let ``dot product of two tuples`` () =
    let a = vector 1.0 2.0 3.0
    let b = vector 2.0 3.0 4.0
    Assert.ValEqual(20.0, dot a b)

[<Fact>]
let ``cross product of two tuples`` () =
    let a = vector 1.0 2.0 3.0
    let b = vector 2.0 3.0 4.0
    Assert.Equal(vector -1.0 2.0 -1.0, cross a b)
    Assert.Equal(vector 1.0 -2.0 1.0, cross b a)
