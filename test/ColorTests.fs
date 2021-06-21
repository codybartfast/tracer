module ColorTests

open Xunit
open Primitives

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``colors are tuples`` () =
    let c = color -0.5 0.4 1.7
    Assert.Equal(-0.5, r c)
    Assert.Equal(0.4, g c)
    Assert.Equal(1.7, b c)

[<Fact>]
let ``adding two colors`` () =
    let a1 = color 0.9 0.6 0.75
    let a2 = color 0.7 0.1 0.25
    Assert.Equal(color 1.6 0.7 1.0, a1 + a2)

[<Fact>]
let ``subtracting two colors`` () =
    let a1 = color 0.9 0.6 0.75
    let a2 = color 0.7 0.1 0.25
    Assert.Equal(color 0.2 0.5 0.5, a1 - a2)

[<Fact>]
let ``multiplying a color by a scalar`` () =
    let a = color 0.2 0.3 0.4
    Assert.Equal(color 0.4 0.6 0.8, a * 2.0)

[<Fact>]
let ``hadamard product of two colors`` () =
    let a1 = color 1.0 0.2 0.4
    let a2 = color 0.9 1.0 0.1
    Assert.Equal(color 0.9 0.2 0.04, hprod a1 a2)
