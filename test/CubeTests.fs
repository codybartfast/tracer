module CubeTests

open Xunit

open Primitives
open Ray
// open ShapeBase
open Shapes

type Assert = XUnitExtensions.TracerAssert

[<Theory>]
[<InlineData(5.0, 0.5, 0.0, -1, 0, 0, 4, 6)>]
[<InlineData(-5.0, 0.5, 0.0, 1, 0, 0, 4, 6)>]
[<InlineData(0.5, 5.0, 0.0, 0, -1, 0, 4, 6)>]
[<InlineData(0.5, -5.0, 0.0, 0, 1,  0, 4, 6)>]
[<InlineData(0.5, 0.0, 5.0, 0, 0, -1, 4, 6)>]
[<InlineData(0.5, 0.0, -5.0, 0, 0, 1, 4, 6)>]
[<InlineData(0.0, 0.5, 0.0, 0, 0, 1, -1, 1)>]
let `` A ray intersects a cube`` (px, py, pz, vx, vy, vz, t1, t2) =
    let c = Cube()
    let r = ray (point px py pz) (vectori vx vy vz)
    let xs = c.LocalIntersect(r)
    Assert.Equal(2, xs.Length)
    Assert.Equal(t1, xs.[0].T)
    Assert.Equal(t2, xs.[1].T)
    ()

[<Theory>]
[<InlineData(-2, 0, 0, 0.2673, 0.5345, 0.8018)>]
[<InlineData(0, -2, 0, 0.8018, 0.2673, 0.5345)>]
[<InlineData(0, 0, -2, 0.5345, 0.8018, 0.2673)>]
[<InlineData(2, 0, 2, 0.0, 0.0, -1.0)>]
[<InlineData(0, 2, 2, 0.0, -1.0, 0.0)>]
[<InlineData(2, 2, 0, -1.0, 0.0, 0.0)>]
let ``A ray misses a cube`` (px, py, pz, vx, vy, vz) =
    let c = Cube()
    let r = ray (pointi px py pz) (vector vx vy vz)
    let xs = c.LocalIntersect(r)
    Assert.Equal(0, xs.Length)

[<Theory>]
[<InlineData(1.0, 0.5, -0.8, 1, 0, 0)>]
[<InlineData(-1.0, -0.2, 0.9, -1, 0, 0)>]
[<InlineData(-0.4, 1.0, -0.1, 0, 1, 0)>]
[<InlineData(0.3, -1.0, -0.7, 0, -1, 0)>]
[<InlineData(-0.6, 0.3, 1.0, 0, 0, 1)>]
[<InlineData(0.4, 0.4, -1.0, 0, 0, -1)>]
[<InlineData(1.0, 1.0, 1.0, 1, 0, 0)>]
[<InlineData(-1.0, -1.0, -1.0, -1, 0, 0)>]
let ``The normal on the surface of a cube`` (px, py, pz, vx, vy, vz) =
    let c = Cube()
    let p = point px py pz
    let norm = c.LocalNormalAt(p)
    Assert.Equal(vectori vx vy vz, norm)
