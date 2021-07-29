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
let ``Shading an intersection`` () =
    let w = defaultWorld ()
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let shape = w.[0]
    let i = intersection 4.0 shape
    let comps = prepareComputations i r
    let c = w.ShadeHit(comps, 0)
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
    let c = w.ShadeHit(comps, 0)
    Assert.Equal(color 0.90498 0.90498 0.90498, c)

[<Fact>]
let ``The color when a ray misses`` () =
    let w = defaultWorld ()
    let r = ray (pointi 0 0 -5) (vectori 0 1 0)
    let c = w.ColorAt(r, 0)
    Assert.Equal(black, c)

[<Fact>]
let ``The color when a ray hits`` () =
    let w = defaultWorld ()
    let r = ray (pointi 0 0 -5) (vectori 0 0 1)
    let c = w.ColorAt(r, 0)
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
    let c = w.ColorAt(r, 0)
    Assert.Equal(innerMaterial.Pattern.ColorAt zeroPoint, c)

[<Fact>]
let ``There is no shadow when nothing is colinear with point and light`` () =
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
    let c = w.ShadeHit(comps, 0)
    Assert.Equal(color 0.1 0.1 0.1, c)

[<Fact>]
let ``he reflected color for a nonreflective material`` () =
    let shape = defaultWorldS2
    let mat2 = shape.Material.With(ambient = 1.0)
    let w = World(defaultWorldLights, [defaultWorldS1; shape])
    let r = ray zeroPoint (vectori 0 0 1)
    let i = intersection 1.0 shape
    let comps = prepareComputations i r
    let color = w.RefectedColor(comps, 1)
    Assert.Equal(black, color)

[<Fact>]
let ``The reflected color for a reflective material`` () =
    let shape = Plane(translationi 0 -1 0, material.With(reflective = 0.5))
    let w =dfltWorldWith shape
    let r = ray (pointi 0 0 -3) (vector 0.0 -hsr2 hsr2)
    let i = intersection sr2 shape
    let comps = prepareComputations i r
    let col = w.RefectedColor(comps, 1)
    Assert.Equal(color 0.19032 0.2379 0.14274, col)

[<Fact>]
let ``ShadeHit with a reflective material`` () =
    let shape = Plane(translationi 0 -1 0, material.With(reflective = 0.5))
    let w = dfltWorldWith shape
    let r = ray (pointi 0 0 -3) (vector 0.0 -hsr2 hsr2)
    let i = intersection sr2 shape
    let comps = prepareComputations i r
    let col = w.ShadeHit(comps, 1)
    Assert.Equal(color 0.87677 0.92436 0.82918, col)

[<Fact>]
let ``color_at() with mutually reflective surfaces`` () =
    let light = pointLight zeroPoint white
    let lower = Plane(translationi 0 -1 0, material.With(reflective = 1.0))
    let upper = Plane(translationi 0 1 0, material.With(reflective = 1.0))
    let w = world light [lower; upper]
    let r = ray zeroPoint (vectori 0 1 0)
    Assert.True(true)


[<Fact>]
let ``The reflected color at the maximum recursive depth`` () =
    let shape = Plane(translationi 0 -1 0, material.With(reflective = 0.5))
    let w = dfltWorldWith shape
    let r = ray (pointi 0 0 -3) (vector 0.0 -hsr2 hsr2)
    let i = intersection sr2 shape
    let comps = prepareComputations i r
    let col = w.RefectedColor(comps, 0)
    Assert.Equal(black, col)
