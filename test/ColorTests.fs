module ColorTests

open Xunit
open Tuple

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let colors_are_tuples () =
    let c = color -0.5 0.4 1.7
    Assert.Equal(-0.5, red c)
    Assert.Equal(0.4, green c)
    Assert.Equal(1.7, blue c)

[<Fact>]
let adding_two_colors () =
    let a1 = color 0.9 0.6 0.75
    let a2 = color 0.7 0.1 0.25
    Assert.TupleEqual(color 1.6 0.7 1.0, add a1 a2)
    Assert.TupleEqual(color 1.6 0.7 1.0, a1 .+ a2)

[<Fact>]
let subtracting_two_colors () =
    let a1 = color 0.9 0.6 0.75
    let a2 = color 0.7 0.1 0.25
    Assert.TupleEqual(color 0.2 0.5 0.5, sub a1 a2)
    Assert.TupleEqual(color 0.2 0.5 0.5, a1 .- a2)

[<Fact>]
let multiplying_a_color_by_a_scalar () =
    let a = color 0.2 0.3 0.4
    Assert.TupleEqual(color 0.4 0.6 0.8, mul a 2.0)
    Assert.TupleEqual(color 0.4 0.6 0.8, a .* 2.0)

[<Fact>]
let hadamard_product_of_two_colors () =
    let a1 = color 1.0 0.2 0.4
    let a2 = color 0.9 1.0 0.1
    Assert.TupleEqual(color 0.9 0.2 0.04, hprod a1 a2)
