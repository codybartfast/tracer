module PlaneTests

open Xunit

open Primitives
open Ray
open Shapes

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``The normal of a plane is constant everywhere`` () =
    let p = plane ()
    let n1 = p.LocalNormalAt(zeroPoint)
    let n2 = p.LocalNormalAt(pointi 10 0 -10)
    let n3 = p.LocalNormalAt(pointi -5 0 150)
    Assert.Equal(vectori 0 1 0, n1)
    Assert.Equal(vectori 0 1 0, n2)
    Assert.Equal(vectori 0 1 0, n3)

[<Fact>]
let ``Intersect with a ray parallel to the plane`` () =
    let p = plane ()
    let r = ray (pointi 0 10 0) (vectori 0 0 1)
    let xs = p.LocalIntersect(r)
    Assert.True(xs.IsEmpty)

[<Fact>]
let ``Intersect with a coplanary ray`` () =
    let p = plane ()
    let r = ray (pointi 0 0 0) (vectori 0 0 1)
    let xs = p.LocalIntersect(r)
    Assert.True(xs.IsEmpty)

[<Fact>]
let ``A ray intersecting a plane from above`` () =
    let p = plane ()
    let r = ray (pointi 0 1 0) (vectori 0 -1 0)
    let xs = p.LocalIntersect(r)
    Assert.Equal(1, xs.Length)
    Assert.Equal(1.0, xs.[0].T)
    Assert.Equal(p, xs.[0].Object)

[<Fact>]
let ``A ray intersecting a plane from belo`` () =
    let p = plane ()
    let r = ray (pointi 0 -1 0) (vectori 0 1 0)
    let xs = p.LocalIntersect(r)
    Assert.Equal(1, xs.Length)
    Assert.Equal(1.0, xs.[0].T)
    Assert.Equal(p, xs.[0].Object)

