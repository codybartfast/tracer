module RayTests

open Xunit
open Primitives
open Matrix
open Transform
open Ray

type Assert = XUnitExtensions.TracerAssert

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
    Assert.Equal(point x y z, position r t)

[<Fact>]
let ``Spheres have identity (not in book)`` () =
    let s = sphere ()
    let s' = Sphere ()
    Assert.NotEqual(s, s')
    Assert.True(s <> s')

[<Fact>]
let ``A ray intersects a sphere at two points`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let s = Sphere ()
    let xs = intersect s r
    Assert.Equal(2, xs.Length)
    Assert.Equal(4.0, xs.[0].T)
    Assert.Equal(6.0, xs.[1].T)

[<Fact>]
let ``A ray intersects a sphere at a tangent`` () =
    let r = ray (pointi 0 1 -5) (vectori 0 0 1)
    let s = Sphere()
    let xs = s.Intersect r
    Assert.Equal(2, xs.Length)
    Assert.Equal(5.0, xs.[0].T)
    Assert.Equal(5.0, xs.[1].T)

[<Fact>]
let ``A ray misses a sphere`` () =
    let r = ray (pointi 0 2 -5) (vectori 0 0 1)
    let s = Sphere()
    let xs = intersect s r
    Assert.Equal(0, xs.Length)

[<Fact>]
let ``A ray originates inside a sphere`` () =
    let r = ray zeroPoint (vectori 0 0 1)
    let s = Sphere()
    let xs = s.Intersect r
    Assert.Equal(2, xs.Length)
    Assert.Equal(-1.0, xs.[0].T)
    Assert.Equal(1.0, xs.[1].T)

[<Fact>]
let ``A ray originates behind a sphere`` () =
    let r = ray (pointi 0 0 5) (vectori 0 0 1)
    let s = Sphere()
    let xs = intersect s r
    Assert.Equal(2, xs.Length)
    Assert.Equal(-6.0, xs.[0].T)
    Assert.Equal(-4.0, xs.[1].T)

[<Fact>]
let ``An intersection encapsulates t and object`` () =
    let s = Sphere ()
    let i = intersection 3.5 s
    Assert.Equal(3.5, i.T)
    Assert.Equal(s, i.Object)

[<Fact>]
let ``Aggregating intersections`` () =
    let s = Sphere()
    let i1 = intersection 1.0 s
    let i2 = intersection 2.0 s
    let xs = intersections [i1; i2]
    Assert.Equal(2, xs.Length)
    Assert.Equal(1.0, xs.[0].T)
    Assert.Equal(2.0, xs.[1].T)

[<Fact>]
let ``Intersect sets the object on the intersection`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let s = Sphere()
    let xs = intersect s r
    Assert.Equal(2, xs.Length)
    Assert.Equal(s, xs.[0].Object)
    Assert.Equal(s, xs.[1].Object)

[<Fact>]
let ``The hit, when all intersections have positive t`` () =
    let s = sphere ()
    let i1 = intersection 1.0 s
    let i2 = intersection 2.0 s
    let xs = intersections [i2; i1]
    let i = hit xs
    Assert.Equal (Some i1, i)

[<Fact>]
let ``The hit, when some intersections have negative t`` () =
    let s = sphere ()
    let i1 = intersection -1.0 s
    let i2 = intersection 1.0 s
    let xs = intersections [i2; i1]
    let i = hit xs
    Assert.Equal (Some i2, i)

[<Fact>]
let ``The hit, when all intersections have negative t`` () =
    let s = sphere ()
    let i1 = intersection -2.0 s
    let i2 = intersection -1.0 s
    let xs = intersections [i2; i1]
    let i = hit xs
    Assert.Equal (None, i)

[<Fact>]
let ``The hit is always the lowest nonnegative intersection`` () =
    let s = sphere ()
    let i1 = intersection 5.0 s
    let i2 = intersection 7.0 s
    let i3 = intersection -3.0 s
    let i4 = intersection 2.0 s
    let xs = intersections [i1; i2; i3; i4]
    let i = hit xs
    Assert.Equal (Some i4, i)

[<Fact>]
let ``A sphere's default transformation`` () =
    let s = sphere ()
    Assert.Equal (identity (), s.Transform)

[<Fact>]
let ``Changing a sphere's transformation`` () =
    let s = sphere ()
    let t = translation 2.0 3.0 4.0
    let s = s.WithTransform(t)
    Assert.Equal (t, s.Transform)

[<Fact>]
let ``Intersecting a scaled sphere with a ray`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let s = Sphere(scaling 2.0 2.0 2.0)
    let xs = intersect s r
    Assert.Equal(2, xs.Length)
    Assert.Equal(3.0, xs.[0].T)
    Assert.Equal(7.0, xs.[1].T)

[<Fact>]
let ``Intersecting a translated sphere with a ray`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let s = Sphere(translation 5.0 0.0 0.0)
    let xs = intersect s r
    Assert.Equal(0, xs.Length)
