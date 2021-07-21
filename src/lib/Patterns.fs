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
type PlanePattern (color: Color) =
    inherit Pattern (identity ())
    member _.Color = color
    override _.LocalColorAt(_: Point) = color
    override a.Equals b =
        match b with
        | :? PlanePattern as b -> a.Color = b.Color
        | _ -> false
    override _.GetHashCode() = 0

(* Stripe Pattern *)
type StripePattern (transform: Matrix, a: Color,  b: Color) =
    inherit Pattern (transform)
    member _.A = a
    member _.B = b
    override p.LocalColorAt(point: Point) =
        match (point.X |> floor |> int) % 2 with
        | 0 -> p.A
        | _ -> p.B

let stripePattern a b = StripePattern(identity (), a, b)
