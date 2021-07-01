module Primitives

open System

// for untyped representations of a primitive type
type Bare = float[]

let epsilon = 0.00001
let wPoint = 1.0
let wVector = 0.0

(* triple functions *)
let valEqual a b = a - b |> abs |> (>) epsilon
let inline private equal (struct(x, y, z)) (struct(x', y', z')) =
    let valEqual a b = a - b |> abs |> (>) epsilon
    (valEqual x x')
    && (valEqual y y')
    && (valEqual z z')
let inline private add (struct(x, y, z)) (struct(x', y', z')) =
    (struct(x + x', y + y', z + z'))
let inline private sub (struct(x, y, z)) (struct(x', y', z')) =
    (struct(x - x', y - y', z - z'))
let inline private scale (struct(x, y, z)) n = struct (x * n, y * n, z * n)
let inline private prod (struct(x, y, z)) (struct(x', y', z')) =
    struct (x * x', y * y', z * z')
let inline private div (struct(x, y, z)) n = struct (x / n, y / n, z / n)

let inline bare x y z w = [| x; y; z; w |]
let inline barei x y z w = [| (float x); (float y); (float z); (float w) |]
let zeroBare = barei 0 0 0 0


(* Vector *)
[<CustomEquality>][<NoComparison>]
type Vector = Vector of (struct (float * float * float)) with
    static member X (Vector (x, _, _)) = x
    static member Y (Vector (_, y, _)) = y
    static member Z (Vector (_, _, z)) = z
    static member (+) ((Vector p), (Vector v)) = Vector (add p v)
    static member (-) ((Vector p), (Vector v)) = Vector (sub p v)
    static member (~-) (Vector (x, y, z)) = Vector (-x, -y, -z)
    static member (*) ((Vector p), n) = Vector (scale p n)
    static member (/) ((Vector p), n) = Vector (div p n)
    static member ToBare (Vector (x, y, z)) = bare x y z wVector
    override a.Equals b =
        match b with
        | :? Vector as b ->
            let (Vector a) = a
            let (Vector b) = b
            equal a b
        | _ -> false
    override _.GetHashCode () = 0

// vector functions
let inline vector x y z = Vector (x, y, z)
let inline vectori x y z = vector (float x) (float y) (float z)
let zeroVector = vectori 0 0 0
let inline toVector (b: Bare) =
    match b with
    | [| x; y; z; w |] when (valEqual w wVector) -> vector x y z
    | [| _; _; _; w |] -> failwith $"Unexpected w for vector: {w}"
    | _ -> failwith $"Unexpected length for bare point: {Array.length b}"
let inline toVectorUnchecked (b: Bare) =
    match b with
    | [| x; y; z; _ |] -> vector x y z
    | _ -> failwith $"Unexpected length for bare point: {Array.length b}"
let inline mag (Vector (x, y, z)) = (x * x) + (y * y) + (z * z) |> sqrt
let inline normalize ((Vector (x, y, z)) as v) =
    let m = mag v in vector (x / m) (y / m) (z / m)
let inline dot (Vector (x, y, z)) (Vector (x', y', z')) =
    (x * x') + (y * y') + (z * z')
let inline cross (Vector (x, y, z)) (Vector (x', y', z')) =
    vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

(* Point *)
[<CustomEquality>][<NoComparison>]
type Point = Point of (struct (float * float * float)) with
    static member X (Point (x, _, _)) = x
    static member Y (Point (_, y, _)) = y
    static member Z (Point (_, _, z)) = z
    static member Equals ((Point a), (Point b)) = equal a b
    static member (+) ((Point p), (Vector v)) = Point (add p v)
    static member (-) ((Point p), (Point v)) = Vector (sub p v)
    static member (-) ((Point p), (Vector v)) = Point (sub p v)
    static member ToBare (Point (x, y, z)) = bare x y z wPoint
    override a.Equals b =
        match b with
        | :? Point as b ->
            let (Point a) = a
            let (Point b) = b
            equal a b
        | _ -> false
    override _.GetHashCode () = 0

// point functions
let inline point x y z = Point (x, y, z)
let inline pointi x y z = point (float x) (float y) (float z)
let zeroPoint = pointi 0 0 0
let inline toPoint (b: Bare) =
    match b with
    | [| x; y; z; w |] when (valEqual w wPoint) -> point x y z
    | [| _; _; _; w |] -> failwith $"Unexpected w for point: {w}"
    | _ -> failwith $"Unexpected length for bare point: ${Array.length b}"


(* Exotic *)
type Exotic = Exotic of (struct (float * float * float * float)) with
    static member ToBare (Exotic (x, y, z, w)) = [|x; y; z; w|]
let exotic x y z w = Exotic (x, y, z, w)


(* Polymorphic functions *)
let inline x (prim: ^T) =
    (^T: (static member X: ^T -> float) (prim))
let inline y (prim: ^T) =
    (^T: (static member Y: ^T -> float) (prim))
let inline z (prim: ^T) =
    (^T: (static member Z: ^T -> float) (prim))
let inline toBare (a: ^T) =
    (^T: (static member ToBare: ^T -> Bare) (a))


(* Color *)
[<Struct>][<CustomEquality>][<NoComparison>]
type Color(r: float, g: float, b: float) =
    member _.Triple = struct (r, g, b)
    member _.R = r
    member _.G = g
    member _.B = b
    static member Equals (a: Color, b: Color) = equal a.Triple b.Triple
    static member (+) (a: Color, b: Color) =
        let struct (r, g, b) = add a.Triple b.Triple
        Color (r, g, b)
    static member (-) (a: Color, b: Color) =
        let struct (r, g, b) = sub a.Triple b.Triple
        Color (r, g, b)
    static member (*) (a: Color, n: float) =
        let struct (r, g, b) = scale a.Triple n
        Color (r, g, b)
    static member (*) (a: Color, n: int) = (*) a (float n)
    static member (*) (a: Color, b: Color) =
        let struct (r, g, b) = prod a.Triple b.Triple
        Color (r, g, b)
    static member (/) (a: Color, n: float) =
        let struct (r, g, b) = div a.Triple n
        Color (r, g, b)
    static member (/) (a: Color, n: int) = (/) a (float n)
    override a.Equals b =
        match b with
        | :? Color as b -> equal a.Triple b.Triple
        | _ -> false
    override _.GetHashCode () = 0
    override _.ToString () = $"Color {r} {g} {b}"

// color functions
let inline color r g b = Color (r, g, b)
let inline colori r g b = Color ((float r), (float g), (float b))
let inline r (c: Color) = c.R
let inline g (c: Color) = c.G
let inline b (c: Color) = c.B
let inline hprod (c: Color) (d: Color) =
    color (c.R * d.R) (c.G * d.G) (c.B * d.B)

let black = color 0.0 0.0 0.0
let white = color 1.0 1.0 1.0

let mix (colors: Color list) = (List.reduce (+) colors) / colors.Length
let lighten c = mix[white; c]
let darken (c: Color) = c / 2

let grey = lighten black
let lightGrey = lighten grey
let darkGrey = darken grey

let red = color 1.0 0.0 0.0
let lightRed = lighten red
let darkRed = darken red

let green = color 0.0 1.0 0.0
let lightGreen = lighten green
let darkGeen = darken green

let blue = color 0.0 0.0 1.0
let lightBlue = lighten blue
let darkBlue = darken blue

let yellow = red + green
let lightYellow = lighten yellow
let darkYellow = darken yellow

let magenta = red + blue
let lightMagenta = lighten magenta
let darkMagenta = darken magenta

let cyan = green + blue
let lightCyan = lighten cyan
let darkCyan = darken cyan
