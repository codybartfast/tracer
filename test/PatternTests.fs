module PatternTests

open Xunit

open Matrix
open Primitives
open Patterns
open ShapeBase
open Shapes
open Transformations

open TestPattern

type Assert = XUnitExtensions.TracerAssert

[<Fact>]
let ``Creating a stripe pattern`` () =
    let pattern = StripePattern(white, black)
    Assert.Equal(white, pattern.A)
    Assert.Equal(black, pattern.B)

[<Fact>]
let ``A stripe pattern is constant in y`` () =
    let pattern = StripePattern(white, black)
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 0))
    Assert.Equal(white, pattern.ColorAt(pointi 0 1 0))
    Assert.Equal(white, pattern.ColorAt(pointi 0 2 0))

[<Fact>]
let ``A stripe pattern is constant in z`` () =
    let pattern = StripePattern(white, black)
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 0))
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 1))
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 2))

[<Fact>]
let ``A stripe pattern alternates in x`` () =
    let pattern = StripePattern(white, black)
    Assert.Equal(white, pattern.ColorAt(pointi 0 0 0))
    Assert.Equal(white, pattern.ColorAt(point 0.9 0.0 0.0))
    Assert.Equal(black, pattern.ColorAt(pointi 1 0 0))
    Assert.Equal(black, pattern.ColorAt(point -0.1 0.0 0.0))
    Assert.Equal(black, pattern.ColorAt(pointi -1 0 0))
    Assert.Equal(white, pattern.ColorAt(point -1.1 0.0 0.0))

[<Fact>]
let ``Stripes with an object transformation`` () =
    let material = defaultMaterial.With(pattern = StripePattern(white, black))
    let object = Sphere(scalingi 2 2 2, material)
    let c = object.ColorAt(point 1.5 0.0 0.0)
    Assert.Equal(white, c)

[<Fact>]
let ``Stripes with a pattern transformation`` () =
    let pattern = StripePattern(scalingi 2 2 2, white, black)
    let material = defaultMaterial.With(pattern = pattern)
    let object = Sphere(material = material)
    let c = object.ColorAt(point 1.5 0.0 0.0)
    Assert.Equal(white, c)

[<Fact>]
let ``Stripes with both and object and a pattern transformation`` () =
    let pattern = StripePattern(translation 0.5 0.0 0.0, white, black)
    let material = defaultMaterial.With(pattern = pattern)
    let object = Sphere(scalingi 2 2 2, material)
    let c = object.ColorAt(point 2.5 0.0 0.0)
    Assert.Equal(white, c)

[<Fact>]
let ``The default pattern transformation`` () =
    let pattern = TestPattern()
    Assert.Equal(identity (), pattern.Transform)

[<Fact>]
let ``Assigning a transformation`` () =
    let transform = translationi 1 2 3
    let pattern = TestPattern(transform)
    Assert.Equal(transform, pattern.Transform)

[<Fact>]
let ``A pattern with an object transformation`` () =
    let material = defaultMaterial.With(pattern = TestPattern())
    let shape = Sphere(scalingi 2 2 2, material)
    Assert.Equal(color 1.0 1.5 2.0, shape.ColorAt(pointi 2 3 4))

[<Fact>]
let ``A pattern with an pattern transformation`` () =
    let material = defaultMaterial.With(pattern = TestPattern(scalingi 2 2 2))
    let shape = Sphere(material = material)
    Assert.Equal(color 1.0 1.5 2.0, shape.ColorAt(pointi 2 3 4))

[<Fact>]
let ``A pattern with both an object and a pattern transformation`` () =
    let material =
        defaultMaterial.With(pattern = TestPattern(translation 0.5 1.0 1.5))
    let shape = Sphere(scalingi 2 2 2, material)
    Assert.Equal(color 0.75 0.5 0.25, shape.ColorAt(point 2.5 3.0 3.5))

[<Fact>]
let ``A gradient linearly interpolates between colors`` () =
    let pattern = GradientPattern(white, black)
    Assert.Equal(white, pattern.ColorAt(zeroPoint))
    Assert.Equal(lightGrey, pattern.ColorAt(point 0.25 0.0 0.0))
    Assert.Equal(grey, pattern.ColorAt(point 0.50 0.0 0.0))
    Assert.Equal(darkGrey, pattern.ColorAt(point 0.75 0.0 0.0))

[<Fact>]
let ``A ring should extend in both x and y`` () =
    let pattern = RingPattern(white, black)
    Assert.Equal(white, pattern.ColorAt(zeroPoint))
    Assert.Equal(black, pattern.ColorAt(pointi 1 0 0))
    Assert.Equal(black, pattern.ColorAt(pointi 0 0 1))
    Assert.Equal(black, pattern.ColorAt(point 0.708 0.0 0.708))

[<Fact>]
let ``Checkers should repeat in x`` () =
    let pattern = CheckersPattern(white, black)
    Assert.Equal(white, pattern.ColorAt(zeroPoint))
    Assert.Equal(white, pattern.ColorAt(point 0.99 0.0 0.0))
    Assert.Equal(black, pattern.ColorAt(point 1.01 0.0 0.0))

[<Fact>]
let ``Checkers should repeat in y`` () =
    let pattern = CheckersPattern(white, black)
    Assert.Equal(white, pattern.ColorAt(zeroPoint))
    Assert.Equal(white, pattern.ColorAt(point 0.0 0.99 0.0))
    Assert.Equal(black, pattern.ColorAt(point 0.0 1.01 0.0))

[<Fact>]
let ``Checkers should repeat in z`` () =
    let pattern = CheckersPattern(white, black)
    Assert.Equal(white, pattern.ColorAt(zeroPoint))
    Assert.Equal(white, pattern.ColorAt(point 0.0 0.0 0.99))
    Assert.Equal(black, pattern.ColorAt(point 0.0 0.0 1.01))
