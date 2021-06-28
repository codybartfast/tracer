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
    let s1 = sphere()
                .With(material = material()
                    .With(color = color 0.8 1.0 0.6,
                                  diffuse = 0.7,
                                  specular = 0.2 ))
    let s2 = Sphere(scaling 0.5 0.5 0.5)
    let w = world ()
    Assert.True(w.Contains((equivalent s1)))
    // Assert.True(w.Contains(s1))
