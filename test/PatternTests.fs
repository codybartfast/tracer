module PatternTests

open Xunit

open Primitives
open Patterns

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``Creating a stripe pattern`` () =
    let pattern = stripePattern white black
    Assert.Equal(white, pattern.A)
    Assert.Equal(black, pattern.B)

[<Fact>]
let ``A stripe pattern is constant in y`` () =
    let pattern = stripePattern white black
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 0))
    Assert.Equal(white, pattern.ColorAt(pointi 0 1 0))
    Assert.Equal(white, pattern.ColorAt(pointi 0 2 0))

[<Fact>]
let ``A stripe pattern is constant in z`` () =
    let pattern = stripePattern white black
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 0))
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 1))
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 2))

[<Fact>]
let ``A stripe pattern alternates in x`` () =
    let pattern = stripePattern white black
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 0))
    Assert.Equal(white, pattern.ColorAt(point 0.9 0.0 0.0))
    Assert.Equal(black, pattern.ColorAt(pointi 1 0 0))
    Assert.Equal(black, pattern.ColorAt(point -0.1 0.0 0.0))
    Assert.Equal(black, pattern.ColorAt(pointi -1 0 0))
    Assert.Equal(white, pattern.ColorAt(point -1.1 0.0 0.0))
