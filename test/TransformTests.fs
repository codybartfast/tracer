module TransfromTests

open System
open Xunit

open Matrix
open Transform
open Primitives

type Assert = XUnitExtensions.TracerAssert

let hsr2 = (sqrt 2.0) / 2.0

[<Fact>]
let ``multiplying by a translation matrix`` () =
    let transform = translation 5.0 -3.0 2.0
    let p = point -3.0 4.0 5.0
    Assert.Equal(point 2.0 1.0 7.0 |> toBare, transform * p)

[<Fact>]
let ``multiplying by the inverse of a translation matrix``  () =
    let transform = translation 5.0 -3.0 2.0
    let inv = inverse transform
    let p = point -3.0 4.0 5.0
    Assert.Equal(point -8.0 7.0 3.0, inv *. p)

[<Fact>]
let ``translation does not affect vectors`` () =
    let transform = translation 5.0 -3.0 2.0
    let v = vector -3.0 4.0 5.0
    Assert.Equal(v, transform *. v)

[<Fact>]
let ``a scaling matrix applied to a point`` () =
    let transform = scaling 2.0 3.0 4.0
    let p = point -4.0 6.0 8.0
    Assert.Equal(point -8.0 18.0 32.0, transform *. p)

[<Fact>]
let ``a scaling matrix applied to a vector`` () =
    let transform = scaling 2.0 3.0 4.0
    let v = vector -4.0 6.0 8.0
    Assert.Equal(vector -8.0 18.0 32.0, transform *. v)

[<Fact>]
let ``multiplying by the inverse of a scaling matrix`` () =
    let transform = scaling 2.0 3.0 4.0
    let inv = inverse transform
    let v = vector -4.0 6.0 8.0
    Assert.Equal(vector -2.0 2.0 2.0, inv *. v)

[<Fact>]
let ``reflection is scaling by a negative value`` () =
    let transform = scaling -1.0 1.0 1.0
    let p = point 2.0 3.0 4.0
    Assert.Equal(point -2.0 3.0 4.0, transform *. p)

[<Fact>]
let ``rotating a point around the x axis`` () =
    let p = pointi 0 1 0
    let halfQuater = rotationX (pi / 4.0)
    let fullQuater = rotationX (pi / 2.0)
    Assert.Equal(point 0.0 hsr2 hsr2, halfQuater *. p)
    Assert.Equal(pointi 0 0 1, fullQuater *. p)

[<Fact>]
let ``the inverse of an x rotation rotates in the opposite direction`` () =
    let p = pointi 0 1 0
    let halfQuater = rotationX (pi / 4.0)
    let inv = inverse halfQuater
    Assert.Equal(point 0.0 hsr2 -hsr2, inv *. p)

[<Fact>]
let ``rotating a point around the y axis`` () =
    let p = pointi 0  0 1
    let halfQuater = rotationY (pi / 4.0)
    let fullQuater = rotationY (pi / 2.0)
    Assert.Equal(point hsr2 0.0 hsr2, halfQuater *. p)
    Assert.Equal(pointi 1 0 0, fullQuater *. p)

[<Fact>]
let ``rotating a point around the z axis`` () =
    let p = pointi 0  1 0
    let halfQuater = rotationZ (pi / 4.0)
    let fullQuater = rotationZ (pi / 2.0)
    Assert.Equal(point -hsr2 hsr2 0.0, halfQuater *. p)
    Assert.Equal(pointi -1 0 0, fullQuater *. p)

[<Fact>]
let ``a shearing transformation moves x in proportion to y`` () =
    let transform = shearing 1.0 0.0 0.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 5 3 4, transform *. p)

[<Fact>]
let ``a shearing transformation moves x in proportion to z`` () =
    let transform = shearing 0.0 1.0 0.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 6 3 4, transform *. p)

[<Fact>]
let ``a shearing transformation moves y in proportion to x`` () =
    let transform = shearing 0.0 0.0 1.0 0.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 2 5 4, transform *. p)

[<Fact>]
let ``a shearing transformation moves y in proportion to z`` () =
    let transform = shearing 0.0 0.0 0.0 1.0 0.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 2 7 4, transform *. p)

[<Fact>]
let ``a shearing transformation moves z in proportion to x`` () =
    let transform = shearing 0.0 0.0 0.0 0.0 1.0 0.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 2 3 6, transform *. p)

[<Fact>]
let ``a shearing transformation moves z in proportion to y`` () =
    let transform = shearing 0.0 0.0 0.0 0.0 0.0 1.0
    let p = pointi 2 3 4
    Assert.Equal(pointi 2 3 7, transform *. p)

[<Fact>]
let ``individual transformations are applied in sequence`` () =
    let p = pointi 1 0 1
    let a = rotationX (pi / 2.0)
    let b = scaling 5.0 5.0 5.0
    let c = translation 10.0 5.0 7.0
    let p2 = a *. p
    Assert.Equal(pointi 1 -1 0, p2)
    let p3 = b *. p2
    Assert.Equal(pointi 5 -5 0, p3)
    let p4 = c *. p3
    Assert.Equal(pointi 15 0 7, p4)

[<Fact>]
let ``chained transformations must be applied in reverse order`` () =
    let p = pointi 1 0 1
    let r = rotationX (pi / 2.0)
    let s = scaling 5.0 5.0 5.0
    let t = translation 10.0 5.0 7.0
    let chain = t * s * r
    Assert.Equal(pointi 15 0 7, chain *. p)

[<Fact>]
let ``fluent transformations 1`` () =
    let t =
        identity ()
        |* rotationX (pi / 2.0)
        |* scaling 5.0 5.0 5.0
        |* translation 10.0 5.0 7.0
    Assert.Equal(pointi 15 0 7, t *. pointi 1 0 1)

[<Fact>]
let ``fluent transformations 2`` () =
    pointi 1 0 1
    |*. rotationX (pi / 2.0)
    |*. scaling 5.0 5.0 5.0
    |*. translation 10.0 5.0 7.0
    |> (fun a -> Assert.Equal(pointi 15 0 7, a))

[<Fact>]
let ``fluent transformations 3`` () =
    pointi 1 0 1
    |* rotationX (pi / 2.0)
    |* scaling 5.0 5.0 5.0
    |* translation 10.0 5.0 7.0
    |> toPoint
    |> (fun a -> Assert.Equal(pointi 15 0 7, a))

[<Fact>]
let ``fluent transformations 4`` () =
    pointi 1 0 1
    |* rotationX (pi / 2.0)
    |* scaling 5.0 5.0 5.0
    |* translation 10.0 5.0 7.0
    |> (fun a -> Assert.Equal(barei 15 0 7 1, a))

[<Fact>]
let ``The transformation matrix for the default orientation`` () =
    let from = pointi 0 0 0
    let ``to`` = pointi 0 0 -1
    let up = vectori 0 1 0
    let t =  viewTransform from ``to`` up
    Assert.Equal(identity (), t)

[<Fact>]
let ``The transformation matrix lookin in positive z direction`` () =
    let from = pointi 0 0 0
    let ``to`` = pointi 0 0 1
    let up = vectori 0 1 0
    let t = viewTransform from ``to`` up
    Assert.Equal(scalingi -1 1 -1, t)

[<Fact>]
let ``The view tranformatin moves the world`` () =
    let from = pointi 0 0 8
    let ``to`` = pointi 0 0 0
    let up = vectori 0 1 0
    let t = viewTransform from ``to`` up
    Assert.Equal(translationi 0 0 -8, t)

[<Fact>]
let ``An arbitrary view transformation`` () =
    let from = pointi 1 3 2
    let ``to`` = pointi 4 -2 8
    let up = vectori 1 1 0
    let t = viewTransform from ``to`` up
    let expected =
        Matrix [ [ -0.50709; 0.50709;  0.67612; -2.36643 ]
                 [  0.76772; 0.60609;  0.12122; -2.82843 ]
                 [ -0.35857; 0.59761; -0.71714;  0.00000 ]
                 [  0.00000; 0.00000;  0.00000;  1.00000 ] ]
    Assert.Equal(expected, t)
