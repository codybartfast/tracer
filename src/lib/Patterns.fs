module Patterns

open Primitives

[<AbstractClass>]
type Pattern () =
    abstract member ColorAt : Point -> Color
    override a.Equals b =
        failwith "Pattern equality only supported for PlanePattern"
    override _.GetHashCode () = 0

(* Plain Pattern *)
type PlanePattern (color: Color) =
    inherit Pattern ()
    member _.Color = color
    override _.ColorAt(_: Point) = color
    override a.Equals b =
        match b with
        | :? PlanePattern as b -> a.Color = b.Color
        | _ -> false

(* Stripe Pattern *)
type StripePattern (a: Color,  b: Color) =
    inherit Pattern ()
    member _.A = a
    member _.B = b
    override p.ColorAt(point: Point) =
        match (point.X |> floor |> int) % 2 with
        | 0 -> p.A
        | _ -> p.B

let stripePattern a b = StripePattern(a, b)

// let stripeAt (pattern: StripePattern) (point: Point) =
//     match (point.X |> floor |> int) % 2 with
//     | 0 -> pattern.A
//     | _ -> pattern.B
