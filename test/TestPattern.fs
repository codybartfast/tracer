module TestPattern

open Matrix
open Patterns
open Primitives

[<Sealed>]
type TestPattern (?transform: Matrix) =
    inherit Pattern(defaultArg transform (identity ()))

    override _.LocalColorAt(point) =
        color (x point) (y point) (z point)

    override p.TransformWith(t) =
        TestPattern(t * p.Transform) :> Pattern
