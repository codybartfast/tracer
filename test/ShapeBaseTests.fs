module ShapeTests

open Xunit

open Primitives
open Matrix
open Ray
open ShapeBase
open TestShape
open Transformations

type Assert = XUnitExtensions.TracerAssert

let hsr2 = (sqrt 2.0) / 2.0

[<Fact>]
let ``The default transformation`` () =
    let s = testShape ()
    Assert.Equal(identity (), s.Transform)

[<Fact>]
let ``Assigning a transformation`` () =
    let transform = translationi 2 3 4
    let s = TestShape(transform)
    Assert.Equal(transform, s.Transform)

[<Fact>]
let ``The default material`` () =
    let s = testShape ()
    Assert.Equal(material, s.Material)

[<Fact>]
let ``Assigning a material`` () =
    let m = defaultMaterial.With(ambient = 1.0)
    let s = TestShape(material = m)
    Assert.Equal(m, s.Material)

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
let ``Computing the normal on a translated shape`` () =
    let s = TestShape(translationi 0 1 0)
    let n = normalAt s (point 0.0 1.70711 -0.70711)
    Assert.Equal(vector 0.0 0.70711 -0.70711, n)

[<Fact>]
let ``Computing the normal on a transformed shape`` () =
    let s = TestShape(scaling 1.0 0.5 1.0 * rotationZ (pi / 5.0))
    let n = normalAt s (point 0.0 hsr2 -hsr2)
    Assert.Equal(vector 0.0 0.97014 -0.24254, n)
