module ShadeTests

open Xunit
open Primitives
open ShapeBase
open Shapes

type Assert = XUnitExtensions.TracerAssert

let tsr3 = (sqrt 3.0 ) / 3.0
let hsr2 = (sqrt 2.0) / 2.0

[<Fact>]
let ``The normal on a sphere at a point on the x axis`` () =
    let s = sphere ()
    let n = s.LocalNormalAt(pointi 1 0 0)
    Assert.Equal(vectori 1 0 0, n)

[<Fact>]
let ``The normal on a sphere at a point on the y axis`` () =
    let s = sphere ()
    let n = s.LocalNormalAt(pointi 0 1 0)
    Assert.Equal(vectori 0 1 0, n)

[<Fact>]
let ``The normal on a sphere at a point on the z axis`` () =
    let s = sphere ()
    let n = s.LocalNormalAt(pointi 0 0 1)
    Assert.Equal(vectori 0 0 1, n)

[<Fact>]
let ``The normal on a sphere at a nonaxial point`` () =
    let s = sphere ()
    let n = s.LocalNormalAt(pointi tsr3 tsr3 tsr3)
    Assert.Equal(vectori  tsr3 tsr3 tsr3, n)

[<Fact>]
let ``The normal is a normalized vector`` () =
    let s = sphere ()
    let n = s.LocalNormalAt(pointi tsr3 tsr3 tsr3)
    Assert.Equal(normalize n, n)

(* Replaced with Shape tests..

[<Fact>]
let ``Computing the normal on a translated sphere`` () =
    let s = Sphere(translationi 0 1 0)
    let n = normalAt s (point 0.0 1.70711 -0.70711)
    Assert.Equal(vector 0.0 0.70711 -0.70711, n)

[<Fact>]
let ``Computing the normal on a transformed sphere`` () =
    let s = sphere ()
    let m = scaling 1.0 0.5 1.0 * rotationZ (pi / 5.0)
    let s = s.With(transform = m)
    let n = normalAt s (point 0.0 hsr2 -hsr2)
    Assert.Equal(vector 0.0 0.97014 -0.24254, n)
*)

[<Fact>]
let ``Reflecting a vector approaching at 45°`` () =
    let v = vectori 1 -1 0
    let n = vectori 0 1 0
    let r = reflect v n
    Assert.Equal(vectori 1 1 0, r)

[<Fact>]
let ``Reflecting a vector approaching at 45 degrees`` () =
    let v = vectori 0 -1 0
    let n = vector hsr2 hsr2 0.0
    let r = reflect v n
    Assert.Equal(vectori 1 0 0, r)

[<Fact>]
let ``A point light has a position and intensity`` () =
    let intensity = colori 1 1 1
    let position = pointi 0 0 0
    let light = pointLight position intensity
    Assert.Equal(position, light.Position)
    Assert.Equal(intensity, light.Intensity)

[<Fact>]
let ``A sphere has a default material`` () =
    let s = sphere ()
    let m = s.Material
    Assert.Equal(material (), m)

[<Fact>]
let ``A sphere may be assigned a material`` () =
    let s = sphere ()
    let m = material ()
    let m = m.With(ambient = 1.0)
    let s = s.With(material = m)
    Assert.Equal(m, s.Material)
    Assert.Equal(1.0, s.Material.Ambient)
