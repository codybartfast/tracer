module CameraTests

open Xunit

open Camera
open Canvas
open Matrix
open Primitives
open Transformations
open World

type Assert = XUnitExtensions.TracerAssert

let halfPi = pi / 2.0
let hsr2 = (sqrt 2.0) / 2.0

[<Fact>]
let ``Constructing a camera`` () =
    let hsize = 160
    let vsize = 120
    let fieldOfView = halfPi
    let c = Camera(hsize, vsize, fieldOfView)
    Assert.Equal(160, c.HSize)
    Assert.Equal(120, c.VSize)
    Assert.Equal(halfPi, c.FieldOfView)
    Assert.Equal(identity (), c.Transform)

[<Fact>]
let ``The pixel size for a horizontal canvas`` () =
    let c = Camera(200, 125, halfPi)
    Assert.Equal(0.01, c.PixelSize)

[<Fact>]
let ``The pixel size for a vertical canvas`` () =
    let c = Camera(125, 200, halfPi)
    Assert.Equal(0.01, c.PixelSize)

[<Fact>]
let ``Constructing a ray through the center of the canvas`` () =
    let c = Camera(201, 101, halfPi)
    let r = rayForPixel c 100 50
    Assert.Equal(zeroPoint, r.Origin)
    Assert.Equal(vectori 0 0 -1, r.Direction)

[<Fact>]
let ``Constructing a ray through a corner of the canvas`` () =
    let c = Camera(201, 101, halfPi)
    let r = rayForPixel c 0 0
    Assert.Equal(zeroPoint, r.Origin)
    Assert.Equal(vector 0.66519 0.33259 -0.66851, r.Direction)

[<Fact>]
let ``Constructing a ray when the canvas is transformed`` () =
    let c = Camera(201, 101, halfPi)
    let t = rotationY (pi / 4.0) * translationi 0 -2 5
    let c  = c.With(transform = t)
    let r = rayForPixel c 100 50
    Assert.Equal(pointi 0 2 -5, r.Origin)
    Assert.Equal(vector hsr2 0.0 -hsr2, r.Direction)

[<Fact>]
let ``Rendering a world with a camera`` () =
    let w = defaultWorld ()
    let from = pointi 0 0 -5
    let ``to`` = zeroPoint
    let up = vectori 0 1 0
    let t = viewTransform from ``to`` up
    let c = camera 11 11 halfPi t
    let image = render c w 0
    Assert.Equal(color 0.38066 0.47583 0.2855, pixelAt image 5 5)
