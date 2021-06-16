module RayTests

open Xunit
open Tuple
open Ray

type Assert = XUnitExtensions.TracerAssert

let pointi x y z = point (float x) (float y) (float z)
let vectori x y z = vector (float x) (float y) (float z)

[<Fact>]
let ``Creating and querying a ray`` () =
    let o = pointi 1 2 3
    let d  = vectori 4 5 6
    let r = ray o d
    Assert.Equal(o, origin r)
    Assert.Equal(d, direction r)

[<Theory>]
[<InlineData(0.0, 2.0, 3.0, 4.0)>]
[<InlineData(1.0, 3.0, 3.0, 4.0)>]
[<InlineData(-1.0, 1.0, 3.0, 4.0)>]
[<InlineData(2.5, 4.5, 3.0, 4.0)>]
let ``Computing a point from a distance`` (t, x, y, z) =
    let r = ray (pointi 2 3 4) (vectori 1 0 0)
    Assert.TupleEqual(point x y z, position r t)

[<Fact>]
let ``Spheres have identity (not in book)`` () =
    let s = Sphere ()
    let s' = Sphere ()
    Assert.NotEqual(s, s')
    Assert.True(s <> s')

[<Fact>]
let ``A ray intersects a sphere at two points`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let s = Sphere ()
    let xs = intersects s r
    Assert.Equal(2, xs.Length)
    Assert.Equal(xs.[0], 4.0)
    Assert.Equal(xs.[1], 6.0)

[<Fact>]
let ``A ray intersects a sphere at a tangent`` () =
    let r = ray (pointi 0 1 -5) (vectori 0 0 1)
    let s = Sphere()
    let xs = s.Intersects r
    Assert.Equal(2, xs.Length)
    Assert.Equal(5.0, xs.[0])
    Assert.Equal(5.0, xs.[1])

[<Fact>]
let ``A ray misses a sphere`` () =
    let r = ray (pointi 0 2 -5) (vectori 0 0 1)
    let s = Sphere()
    let xs = intersects s r
    Assert.Equal(0, xs.Length)

[<Fact>]
let ``A ray originates inside a sphere`` () =
    let r = ray (pointi 0 0 0) (vectori 0 0 1)
    let s = Sphere()
    let xs = s.Intersects r
    Assert.Equal(2, xs.Length)
    Assert.Equal(-1.0, xs.[0])
    Assert.Equal(1.0, xs.[1])

[<Fact>]
let ``A ray originates behind a sphere`` () =
    let r = ray (pointi 0 0 5) (vectori 0 0 1)
    let s = Sphere()
    let xs = intersects s r
    Assert.Equal(2, xs.Length)
    Assert.Equal(-6.0, xs.[0])
    Assert.Equal(-4.0, xs.[1])
