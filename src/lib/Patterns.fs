module Patterns

open System
open System.Drawing

open Matrix
open Primitives
open Transformations

[<AbstractClass>]
type Pattern (transfrom: Matrix) =
    let inverseT = inverse transfrom

    member _.Transform = transfrom

    member p.ColorAt(point: Point) : Color =
        point
        |*. inverseT
        |>  p.LocalColorAt

    abstract member LocalColorAt : Point -> Color
    abstract member TransformWith : Matrix -> Pattern

    static member ( * ) (t: Matrix, p: Pattern) = p.TransformWith(t)

    override _.Equals _ =
        failwith "Pattern equality only supported for PlanePattern"
    override _.GetHashCode () = 0

type CombindedPattern ( transform: Matrix,
                        combine : Color list -> Color,
                        patterns: Pattern list) =
    inherit Pattern(transform)
    new (op, [<ParamArray>] patterns) =
        CombindedPattern(identity (), op, patterns)
    override _.LocalColorAt(point: Point) =
        patterns
        |> List.map (fun pat -> pat.ColorAt(point))
        |> combine
    override p.TransformWith(t) =
        CombindedPattern(t * p.Transform, combine, patterns) :> Pattern

let blendPatterns patterns = CombindedPattern(blendColors, patterns)
let addPatterns patterns = CombindedPattern(addColors, patterns)

(* Solid Pattern *)
type SolidPattern (color: Color) =
    inherit Pattern (identity ())
    member _.Color = color
    override _.LocalColorAt(_: Point) = color
    override p.TransformWith(_) = p :> Pattern
    override _.Equals b =
        match b with
        | :? SolidPattern as b -> color = b.Color
        | _ -> false
    override _.GetHashCode() = 0

let solid color = SolidPattern(color)
let solidWhite, solidBlack = solid white, solid black


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
    override p.TransformWith(t) =
        StripePattern(t * p.Transform, p.A, p.B) :> Pattern

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
    override p.TransformWith(t) =
        GradientPattern(t * p.Transform, p.A, p.B) :> Pattern

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
    override p.TransformWith(t) =
        RingPattern(t * p.Transform, p.A, p.B) :> Pattern

type CheckersPattern (transform: Matrix, a: Pattern,  b: Pattern) =
    inherit Pattern (transform)
    new(a,  b) = CheckersPattern(identity (), a, b)
    member _.A = a
    member _.B = b
    override _.LocalColorAt((Point (x, y, z)) as point) =
        match (abs x + abs y + abs z |> int) % 2 with
        | 0 -> a.ColorAt(point)
        | _ -> b.ColorAt(point)
    override p.TransformWith(t) =
        CheckersPattern(t * p.Transform, p.A, p.B) :> Pattern


(* Not in the book *)

type RgbCubePattern (transform: Matrix) =
    inherit Pattern(transform)
    new() = RgbCubePattern(identity ())
    override _.LocalColorAt(Point (x, y, z)) = color x y z
    override p.TransformWith(t) = RgbCubePattern(t * p.Transform) :> Pattern

type InvMercatorPattern (transform: Matrix, pattern: Pattern) =
    inherit Pattern(transform)
    new(pattern) = InvMercatorPattern(identity (), pattern)

    override p.LocalColorAt(Point (x, y, z) as point) =
        let toAbs1 = min 1.0 >> max -1.0
        let xMax = 1.0 - z * z |> max 0.0 |> sqrt
        let xSin = match xMax with 0.0 -> 1.0 | _ -> toAbs1 (x / xMax)
        let lowLongitude = Math.Asin (xSin)
        let longitude =
            match x >= 0.0, y >= 0.0 with
            | _, true -> lowLongitude
            | true, _ ->  pi - lowLongitude
            | false, _ -> -pi - lowLongitude
        let latitude = z |> toAbs1 |> Math.Asin
        let z' = (latitude / pi + 0.5)
        let x' = (longitude / (2.0 * pi) + 0.5)

        Primitives.point x' 0.0 -z'
            |> pattern.ColorAt

    override p.TransformWith(t) = InvMercatorPattern(t * transform, pattern) :> Pattern

type BitmapPattern (transform: Matrix, bitmap: Bitmap) =
    inherit Pattern(transform)

    new (transform, name: string) = BitmapPattern(transform, new Bitmap(name))
    new (name: string) =
        let bitmap = new Bitmap(name)
        let aspect = float bitmap.Width / float bitmap.Height
        let transform = scaling aspect 1.0 1.0
        BitmapPattern(transform, bitmap)

    override _.LocalColorAt((Point (x, _, z)) as point) =
        let x = (x - floor x) * (float bitmap.Width) |> int
        let y = (z - floor z) * (float bitmap.Height) |> int
        let c = bitmap.GetPixel(x, y)
        let toFloat i = (float i) / 255.0
        color (toFloat c.R) (toFloat c.G) (toFloat c.B)
    override p.TransformWith(t) =
        BitmapPattern(t * p.Transform, bitmap) :> Pattern


(* Not in this book *)

let flipVert (pattern: Pattern) = (scalingi 1 1 -1) * pattern
let flipHoriz (pattern: Pattern) = (scalingi -1 1 1) * pattern
let rotate90 (pattern: Pattern) = (rotationY (pi * 0.5)) * pattern
let rotate180 (pattern: Pattern) = (rotationY pi) * pattern
let rotate270 (pattern: Pattern) = (rotationY (pi * 1.5)) * pattern
let beside a b = StripePattern(scaling 0.5 1.0 1.0, a, b) :> Pattern
let below a b = beside (rotate270 a) (rotate270 b) |> rotate90

let flippedPairs pattern =
    let pattern2 = beside pattern (flipVert pattern)
    below pattern2 pattern2

let rec rightSplit n (pattern: Pattern) : Pattern =
    match n with
    | 0 -> pattern
    | _ ->
        let smaller = rightSplit (n - 1) pattern
        beside pattern (below smaller smaller)

let rec upSplit n (pattern: Pattern) : Pattern =
    match n with
    | 0 -> pattern
    | _ ->
        let smaller = upSplit (n - 1) pattern
        below pattern (beside smaller smaller)

let rec cornerSplit n (pattern: Pattern) =
    match n with
    | 0 -> pattern
    | _ ->
        let up = upSplit (n - 1) pattern
        let right = rightSplit (n - 1) pattern
        let topLeft = beside up up
        let bottomRight = below right right
        let corner = cornerSplit (n - 1) pattern
        beside (below pattern topLeft) (below bottomRight corner)

let rec squareLimit n pattern =
    let quater = cornerSplit n pattern
    let half = beside (flipHoriz quater) quater
    below (flipVert half) half
