module MaterialTests

open Xunit
open Primitives
open Transformations
open ShapeBase
open Sphere

type Assert = XUnitExtensions.TracerAssert

let hsr2 = (sqrt 2.0) / 2.0

let m = material ()
let position = zeroPoint

[<Fact>]
let ``The default material`` () =
    let m = material ()
    Assert.Equal(colori 1 1 1, m.Color)
    Assert.Equal(0.1, m.Ambient)
    Assert.Equal(0.9, m.Diffuse)
    Assert.Equal(0.9, m.Specular)
    Assert.Equal(200.0, m.Shininess)

[<Fact>]
let ``Lighting with the eye between the light and the surface`` () =
    let eyev = vectori 0 0 -1
    let normalv = vectori 0 0 -1
    let light = pointLight (pointi 0 0 -10) (colori 1 1 1)
    let result = lighting m light position eyev normalv false
    Assert.Equal(color 1.9 1.9 1.9, result)


let ``Lighting with the eye between the light and the surface, eye offset 45 degs`` () =
    let eyev = vector 0.0 -hsr2 -hsr2
    let normalv = vectori 0 0 -1
    let light = pointLight (pointi 0 0 -10) (colori 1 1 1)
    let result = lighting m light position eyev normalv false
    Assert.Equal(color 1.0 1.0 1.0, result)

[<Fact>]
let ``Lighting with eye opposite surface, light offset 45 degs`` () =
    let eyev = vectori 0 0 -1
    let normalv = vectori 0 0 -1
    let light = pointLight (pointi 0 10 -10) (colori 1 1 1)
    let result = lighting m light position eyev normalv false
    Assert.Equal(color 0.7364 0.7364 0.7364, result)

[<Fact>]
let ``Lighting with eye in the path of the reflection vector`` () =
    let eyev = vector 0.0 -hsr2 -hsr2
    let normalv = vectori 0 0 -1
    let light = pointLight (pointi 0 10 -10) (colori 1 1 1)
    let result = lighting m light position eyev normalv false
    Assert.Equal(color 1.6364 1.6364 1.6364, result)

[<Fact>]
let ``Lighting with the light behind the surface`` () =
    let eyev = vectori 0 0 -1
    let normalv = vectori 0 0 -1
    let light = pointLight (pointi 0 0 10) (colori 1 1 1)
    let result = lighting m light position eyev normalv false
    Assert.Equal(color 0.1 0.1 0.1, result)

[<Fact>]
let ``Lighting with the surface in shadow`` () =
    let eyev = vectori 0 0 -1
    let normalv = vectori 0 0 -1
    let light = pointLight (pointi 0 0 -10) white
    let inShadow = true
    let result = lighting m light  position eyev normalv inShadow
    Assert.Equal(color 0.1 0.1 0.1, result)
