module WorldTests

open Xunit
open Primitives
open Transform
open Ray
open World

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``Creating a world`` () =
    let w = world ()
    Assert.True(w.IsEmpty)
    Assert.Equal(None, w.Light)

[<Fact>]
let ``The default world`` () =
    let light = pointLight (pointi -10 10 -10) (colori 1 1 1)
    let s1 = Sphere(material = (material ())
                        .With(color = color 0.8 1.0 0.6,
                              diffuse = 0.7,
                              specular = 0.2 ) )
    let s2 = Sphere(scaling 0.5 0.5 0.5)
    let w = defaultWorld ()
    Assert.True(w.Contains(equivalent s1))
    Assert.True(w.Contains(equivalent s2))
    Assert.Equal(Some light, w.Light)

[<Fact>]
let ``Intersect a world with a ray`` () =
    let w = defaultWorld ()
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let xs = intersectWorld w r
    Assert.Equal(4, xs.Length)
    Assert.Equal(4.0, xs.[0].T)
    Assert.Equal(4.5, xs.[1].T)
    Assert.Equal(5.5, xs.[2].T)
    Assert.Equal(6.0, xs.[3].T)
