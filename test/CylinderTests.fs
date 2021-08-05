module CylinderTests

open Xunit

open Primitives
open Ray
// open ShapeBase
open Shapes

type Assert = XUnitExtensions.TracerAssert

[<Theory>]
[<InlineData(1, 0, 0, 0, 1, 0)>]
[<InlineData(0, 0, 0, 0, 1, 0)>]
[<InlineData(0, 0, -5, 1, 1, 1)>]
let `` A ray misses a cylinder`` (px, py, pz, vx, vy, vz) =
    let origin, direction = pointi px py pz, vectori vx vy vz
    let cyl = Cylinder()
    let direction = normalize direction
    let r = ray origin direction
    let xs = cyl.LocalIntersect(r)
    Assert.Equal(0, xs.Length)


[<Theory>]
[<InlineData(1.0, 0.0, -5.0, 0.0, 0.0, 1.0, 5.0, 5.0)>]
[<InlineData(0.0, 0.0, -5.0, 0.0, 0.0, 1.0, 4.0, 6.0)>]
[<InlineData(0.5, 0.0, -5.0, 0.1, 1.0, 1.0, 6.80798, 7.08872)>]
let ``A ray strikes a cylinder`` (px, py, pz, vx, vy, vz, t0, t1) =
    let origin, direction = pointi px py pz, vectori vx vy vz
    let cyl = Cylinder()
    let direction = normalize direction
    let r = ray origin direction
    let xs = cyl.LocalIntersect(r)
    Assert.Equal(2, xs.Length)
    Assert.Equal(t0, xs.[0].T)
    Assert.Equal(t1, xs.[1].T)
