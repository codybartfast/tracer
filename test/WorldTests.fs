module WorldTests

open Xunit

open Primitives
open Ray
open ShapeBase
open Shapes
open Transformations
open World

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``Creating a world`` () =
    let w = World()
    Assert.True(w.IsEmpty)
    Assert.Equal([], w.Lights)

[<Fact>]
let ``The default world`` () =
    let light = pointLight (pointi -10 10 -10) (colori 1 1 1)
    let s1 = Sphere(material = material
                        .With(color = color 0.8 1.0 0.6,
                              diffuse = 0.7,
                              specular = 0.2 ) )
    let s2 = Sphere(scaling 0.5 0.5 0.5)
    let w = defaultWorld ()
    Assert.True(w.Contains(equivalent s1))
    Assert.True(w.Contains(equivalent s2))
    Assert.Equal(light, w.Lights.[0])

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

[<Fact>]
let ``Precomputing the state of an intersection`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let shape = sphere ()
    let i = intersection 4.0 shape
    let comps = prepareComputations i r
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
    let comps = prepareComputations i r
    Assert.Equal(false, comps.Inside)

[<Fact>]
let ``The hit, when an intersection occurs on the inside`` () =
    let r = ray (pointi 0 0 0) (vectori 0 0 1)
    let shape = sphere ()
    let i = intersection 1.0 shape
    let comps = prepareComputations i r
    Assert.Equal(pointi 0 0 1, comps.Point)
    Assert.Equal(vectori 0 0 -1, comps.Eyev)
    Assert.Equal(true, comps.Inside)
    Assert.Equal(vectori 0 0 -1, comps.Normalv)

[<Fact>]
let ``Shading an intersection`` () =
    let w = defaultWorld ()
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let shape = w.[0]
    let i = intersection 4.0 shape
    let comps = prepareComputations i r
    let c = shadeHit w comps
    Assert.Equal(color 0.38066 0.47583 0.2855, c)

[<Fact>]
let ``Shading an intersection from the inside`` () =
    let w =
        (defaultWorld ())
            .With([pointLight (point 0.0 0.25 0.0) (colori 1 1 1)])
    let r = ray (pointi 0 0 0) (vectori 0 0 1)
    let shape = w.[1]
    let i = intersection 0.5 shape
    let comps = prepareComputations i r
    let c = shadeHit w comps
    Assert.Equal(color 0.90498 0.90498 0.90498, c)

[<Fact>]
let ``The color when a ray misses`` () =
    let w = defaultWorld ()
    let r = ray (pointi 0 0 -5) (vectori 0 1 0)
    let c = colorAt w r
    Assert.Equal(black, c)

[<Fact>]
let ``The color when a ray hits`` () =
    let w = defaultWorld ()
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let c = colorAt w r
    Assert.Equal(color 0.38066 0.47583 0.2855, c)

[<Fact>]
let ``The color with an intersection behind the ray`` () =
    let w = defaultWorld ()
    let outer = w.[0] :?> Sphere
    let outerMaterial = outer.Material.With(ambient = 1.0)
    let inner = w.[1] :?> Sphere
    let innerMaterial = inner.Material.With(ambient = 1.0)
    let w =
        w.With(shapes = [
            outer.With(material = outerMaterial)
            inner.With(material = innerMaterial) ])
    let r = ray (point 0.0 0.0 0.75) (vectori 0 0 -1)
    let c = colorAt w r
    Assert.Equal(innerMaterial.Pattern.ColorAt zeroPoint, c)

[<Fact>]
let ``There is no shadow when nothing is collinear with point and light`` () =
    let w = defaultWorld ()
    let p = pointi 0 10 0
    Assert.False(isShadowed w w.FirstLight p)

[<Fact>]
let ``The shadow when an object is between the point and the light`` () =
    let w = defaultWorld ()
    let p = pointi 10 -10 10
    Assert.True(isShadowed w w.FirstLight p)

[<Fact>]
let ``There is no shadow when an object is behind the light`` () =
    let w = defaultWorld ()
    let p = pointi -20 20 -20
    Assert.False(isShadowed w w.FirstLight p)

[<Fact>]
let ``There is no shadow when an object is behind the point`` () =
    let w = defaultWorld ()
    let p = pointi -2 2 -2
    Assert.False(isShadowed w w.FirstLight p)

[<Fact>]
let ``shadeHit is given an intersection in shadow`` () =
    let s2 = Sphere(translationi 0 0 10)
    let w = world (pointLight (pointi 0 0 -10) white) [sphere (); s2]
    let r = ray (pointi 0 0 5) (vectori 0 0 1)
    let i = intersection 4.0 s2
    let comps = prepareComputations i r
    let c = shadeHit w comps
    Assert.Equal(color 0.1 0.1 0.1, c)

[<Fact>]
let ``The hit should offset the point`` () =
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let shape = Sphere (translationi 0 0 1)
    let i = intersection 5.0 shape
    let comps = prepareComputations i r
    Assert.True((comps.OverPoint |> z) < -epsilon/2.0)
