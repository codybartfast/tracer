module Patterns

open Matrix
open Primitives

[<AbstractClass>]
type Pattern (transfrom: Matrix) =
    let inverseT = inverse transfrom

    member _.Transform = transfrom

    member p.ColorAt(point: Point) : Color =
        point
        |*. inverseT
        |>  p.LocalColorAt

    abstract member LocalColorAt : Point -> Color
    override _.Equals _ =
        failwith "Pattern equality only supported for PlanePattern"
    override _.GetHashCode () = 0

(* Plain Pattern *)
type SolidPattern (color: Color) =
    inherit Pattern (identity ())
    member _.Color = color
    override _.LocalColorAt(_: Point) = color
    override _.Equals b =
        match b with
        | :? SolidPattern as b -> color = b.Color
        | _ -> false
    override _.GetHashCode() = 0

(* Stripe Pattern *)
type StripePattern (transform: Matrix, a: Pattern,  b: Pattern) =
    inherit Pattern (transform)
    new(a,  b) = StripePattern(identity (), a, b)
    member _.A = a
    member _.B = b
    override _.LocalColorAt(point: Point) =
        match (point.X |> floor |> int) % 2 with
        | 0 -> a.ColorAt(point)
        | _ -> b.ColorAt(point)

type GradientPattern (transform: Matrix, a: Pattern,  b: Pattern) =
    inherit Pattern (transform)
    new(a,  b) = GradientPattern(identity (), a, b)
    member _.A = a
    member _.B = b
    override _.LocalColorAt(point: Point) =
        let aCol, bCol = a.ColorAt(point), b.ColorAt(point)
        let distance = bCol - aCol
        let fraction = point.X - (floor point.X)
        aCol + distance * fraction

type RingPattern (transform: Matrix, a: Pattern,  b: Pattern) =
    inherit Pattern (transform)
    new(a,  b) = RingPattern(identity (), a, b)
    member _.A = a
    member _.B = b
    override _.LocalColorAt((Point (x, _, z)) as point) =
        let distance = x * x + z * z |> sqrt
        match (floor distance |> int) % 2 with
        | 0 -> a.ColorAt(point)
        | _ -> b.ColorAt(point)

type CheckersPattern (transform: Matrix, a: Pattern,  b: Pattern) =
    inherit Pattern (transform)
    new(a,  b) = CheckersPattern(identity (), a, b)
    member _.A = a
    member _.B = b
    override _.LocalColorAt((Point (x, y, z)) as point) =
        match (abs x + abs y + abs z |> int) % 2 with
        | 0 -> a.ColorAt(point)
        | _ -> b.ColorAt(point)
