module IntersectionTests

open Xunit

open Primitives
open Ray
open ShapeBase
open Shapes
open TestShape
open Transformations

type Assert = XUnitExtensions.TracerAssert

let sr2 = sqrt 2.0
let hsr2 = sr2 / 2.0

[<Fact>]
let ``An intersection encapsulates t and object`` () =
    let s = Sphere ()
    let i = intersection 3.5 s
    Assert.Equal(3.5, i.T)
    Assert.Equal(s, i.Object)

[<Fact>]
let ``Precomputing the state of an intersection`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let shape = sphere ()
    let i = intersection 4.0 shape
    let comps = prepareComputations i r [i]
    Assert.Equal(i.T, comps.T)
    Assert.Equal(i.Object, comps.Object)
    Assert.Equal(pointi 0 0 -1, comps.Point)
    Assert.Equal(vectori 0 0 -1, comps.Eyev)
    Assert.Equal(vectori 0 0 -1, comps.Normalv)


[<Fact>]
let ``The hit, when an intersection occurs on the outside`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let shape = sphere ()
    let i = intersection 4.0 shape
    let comps = prepareComputations i r [i]
    Assert.Equal(false, comps.Inside)

[<Fact>]
let ``The hit, when an intersection occurs on the inside`` () =
    let r = ray (pointi 0 0 0) (vectori 0 0 1)
    let shape = sphere ()
    let i = intersection 1.0 shape
    let comps = prepareComputations i r [i]
    Assert.Equal(pointi 0 0 1, comps.Point)
    Assert.Equal(vectori 0 0 -1, comps.Eyev)
    Assert.Equal(true, comps.Inside)
    Assert.Equal(vectori 0 0 -1, comps.Normalv)


[<Fact>]
let ``The hit should offset the point`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let shape = Sphere (translationi 0 0 1)
    let i = intersection 5.0 shape
    let comps = prepareComputations i r [i]
    Assert.True((comps.OverPoint.Z) < -epsilon/2.0)

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
let ``Intersecting a scaled shape with a ray`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let s = TestShape(scalingi 2 2 2)
    intersect r s |> ignore
    let expected = ray (point 0.0 0.0 -2.5) (vector 0.0 0.0 0.5)
    Assert.Equal(expected, s.SavedRay)

[<Fact>]
let ``Intersecting a translated shape with a ray`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let s = TestShape(translationi 5 0 0)
    intersect r s |> ignore
    let expected = ray (pointi -5 0 -5) (vectori 0 0 1)
    Assert.Equal(expected, s.SavedRay)

[<Fact>]
let ``Precomputing the reflection vector`` () =
    let shape = plane ()
    let r = ray (pointi 0 1 -1) (vector 0.0 -hsr2 hsr2)
    let i = intersection sr2 shape
    let comps = prepareComputations i r [i]
    Assert.Equal(vector 0.0 hsr2 hsr2, comps.Reflectv)

[<Theory>]
[<InlineData(1.0, 1.5, 0)>]
[<InlineData(1.5, 2.0, 1)>]
[<InlineData(2.0, 2.5, 2)>]
[<InlineData(2.5, 2.5, 3)>]
[<InlineData(2.5, 1.5, 4)>]
[<InlineData(1.5, 1.0, 5)>]
let ``Finding n1 and n2 at various intersections`` (n1, n2, idx) =
    let a = Sphere(scalingi 2 2 2, glass.With(refractiveIndex = 1.5))
    let b = Sphere(translation 0.0 0.0 -0.25, glass.With(refractiveIndex = 2.0))
    let c = Sphere(translation 0.0 0.0 0.25, glass.With(refractiveIndex = 2.5))
    let r = ray (pointi 0 0 -4) (vectori 0 0 -1)
    let xs =
        [2.0, a; 2.75, b; 3.25, c; 4.75, b; 5.25, c; 6.0, a]
        |> List.map (fun (t, o) -> intersection t o)
    let comps = xs |> List.map (fun ix -> prepareComputations ix r xs)
    let n1n2 i = (comps.[i].N1, comps.[i].N2)
    Assert.Equal((n1, n2), n1n2 idx)

[<Fact>]
let ``The under point is offset below the surface`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let shape = Sphere(translationi 0 0 1, glass)
    let i = intersection 5.0 shape
    let xs = [i]
    let comps = prepareComputations i r xs
    Assert.True(comps.UnderPoint.Z > epsilon / 2.0)
    Assert.True(comps.Point.Z < comps.UnderPoint.Z)
