module TransfromTests

open System
open Xunit

open Matrix
open Transform
open Primitives

type Assert = XUnitExtensions.TracerAssert

let pointi x y z = point (float x) (float y) (float z)
let vectori x y z = vector (float x) (float y) (float z)
let hsr2 = (Math.Sqrt 2.0) / 2.0

[<Fact>]
let ``multiplying by a translation matrix`` () =
    let transform = translation 5.0 -3.0 2.0
    let p = point -3.0 4.0 5.0
    Assert.Equal(point 2.0 1.0 7.0 |> toBare, transform * p)

[<Fact>]
let ``multiplying by the inverse of a translation matrix`` () =
    let transform = translation 5.0 -3.0 2.0
    let inv = inverse transform
    let p = point -3.0 4.0 5.0
    Assert.Equal(point -8.0 7.0 3.0, inv * p |> toPoint)

[<Fact>]
let ``translation does not affect vectors`` () =
    let transform = translation 5.0 -3.0 2.0
    let v = vector -3.0 4.0 5.0
    Assert.Equal(v, transform * v |> toVector)

[<Fact>]
let ``a scaling matrix applied to a point`` () =
    let transform = scaling 2.0 3.0 4.0
    let p = point -4.0 6.0 8.0
    Assert.Equal(point -8.0 18.0 32.0, transform * p |> toPoint)

[<Fact>]
let ``a scaling matrix applied to a vector`` () =
    let transform = scaling 2.0 3.0 4.0
    let v = vector -4.0 6.0 8.0
    Assert.Equal(vector -8.0 18.0 32.0, transform * v |> toVector)

[<Fact>]
let ``multiplying by the inverse of a scaling matrix`` () =
    let transform = scaling 2.0 3.0 4.0
    let inv = inverse transform
    let v = vector -4.0 6.0 8.0
    Assert.Equal(vector -2.0 2.0 2.0, inv * v |> toVector)

[<Fact>]
let ``reflection is scaling by a negative value`` () =
    let transform = scaling -1.0 1.0 1.0
    let p = point 2.0 3.0 4.0
    Assert.Equal(point -2.0 3.0 4.0, transform * p |> toPoint)

[<Fact>]
let ``rotating a point around the x axis`` () =
    let p = pointi 0 1 0
    let halfQuater = rotationX (pi / 4.0)
    let fullQuater = rotationX (pi / 2.0)
    Assert.Equal(point 0.0 hsr2 hsr2, halfQuater * p |> toPoint)
    Assert.Equal(pointi 0 0 1, fullQuater * p |> toPoint)

[<Fact>]
let ``the inverse of an x rotation rotates in the opposite direction`` () =
    let p = pointi 0 1 0
    let halfQuater = rotationX (pi / 4.0)
    let inv = inverse halfQuater
    Assert.Equal(point 0.0 hsr2 -hsr2, inv * p |> toPoint)

[<Fact>]
let ``rotating a point around the y axis`` () =
    let p = pointi 0  0 1
    let halfQuater = rotationY (pi / 4.0)
    let fullQuater = rotationY (pi / 2.0)
    Assert.Equal(point hsr2 0.0 hsr2, halfQuater * p |> toPoint)
    Assert.Equal(pointi 1 0 0, fullQuater * p |> toPoint)

[<Fact>]
let ``rotating a point around the z axis`` () =
    let p = pointi 0  1 0
    let halfQuater = rotationZ (pi / 4.0)
    let fullQuater = rotationZ (pi / 2.0)
    Assert.Equal(point -hsr2 hsr2 0.0, halfQuater * p |> toPoint)
    Assert.Equal(pointi -1 0 0, fullQuater * p |> toPoint)

[<Fact>]
let ``a shearing transformation moves x in proportion to y`` () =
    let transform = shearing 1.0 0.0 0.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 5 3 4, transform * p |> toPoint)

[<Fact>]
let ``a shearing transformation moves x in proportion to z`` () =
    let transform = shearing 0.0 1.0 0.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 6 3 4, transform * p |> toPoint)

[<Fact>]
let ``a shearing transformation moves y in proportion to x`` () =
    let transform = shearing 0.0 0.0 1.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 2 5 4, transform * p |> toPoint)

[<Fact>]
let ``a shearing transformation moves y in proportion to z`` () =
    let transform = shearing 0.0 0.0 0.0 1.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 2 7 4, transform * p |> toPoint)

[<Fact>]
let ``a shearing transformation moves z in proportion to x`` () =
    let transform = shearing 0.0 0.0 0.0 0.0 1.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 2 3 6, transform * p |> toPoint)

[<Fact>]
let ``a shearing transformation moves z in proportion to y`` () =
    let transform = shearing 0.0 0.0 0.0 0.0 0.0 1.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 2 3 7, transform * p |> toPoint)

[<Fact>]
let ``individual transformations are applied in sequence`` () =
    let p = pointi 1 0 1
    let A = rotationX (pi / 2.0)
    let B = scaling 5.0 5.0 5.0
    let C = translation 10.0 5.0 7.0
    let p2 = A * p
    Assert.Equal(pointi 1 -1 0, p2 |> toPoint)
    let p3 = B * p2
    Assert.Equal(pointi 5 -5 0, p3 |> toPoint)
    let p4 = C * p3
    Assert.Equal(pointi 15 0 7, p4 |> toPoint)

[<Fact>]
let ``chained transformations must be applied in reverse order`` () =
    let p = pointi 1 0 1
    let r = rotationX (pi / 2.0)
    let s = scaling 5.0 5.0 5.0
    let t = translation 10.0 5.0 7.0
    let chain = t * s * r
    Assert.Equal(pointi 15 0 7, chain * p |> toPoint)

[<Fact>]
let ``fluent transformations`` () =
    let t =
        identity ()
        *> rotationX (pi / 2.0)
        *> scaling 5.0 5.0 5.0
        *> translation 10.0 5.0 7.0
    Assert.Equal(pointi 15 0 7, t * pointi 1 0 1 |> toPoint)

[<Fact>]
let ``fluent transformations2`` () =
    pointi 1 0 1
    *> rotationX (pi / 2.0)
    *> scaling 5.0 5.0 5.0
    *> translation 10.0 5.0 7.0
    |> toPoint
    |> (fun a -> Assert.Equal(pointi 15 0 7, a))
