module Primitives

let private epsilon = 0.00001
let private wPoint = 1.0
let private wVector = 0.0

(* tuple functions *)
let valEqual a b = a - b |> abs |> (>) epsilon
let private equal (struct(x, y, z)) (struct(x', y', z')) =
    let valEqual a b = a - b |> abs |> (>) epsilon
    (valEqual x x')
    && (valEqual y y')
    && (valEqual z z')

let private add (struct(x, y, z)) (struct(x', y', z')) =
    (struct(x + x', y + y', z + z'))
let private sub (struct(x, y, z)) (struct(x', y', z')) =
    (struct(x - x', y - y', z - z'))
let private mul (struct(x, y, z)) n = struct (x * n, y * n, z * n)

(* primitive types *)
type Vector = Vector of (struct (float * float * float)) with
    static member X (Vector (x, _, _)) = x
    static member Y (Vector (_, y, _)) = y
    static member Z (Vector (_, _, z)) = z
    static member Equal ((Vector a), (Vector b)) = equal a b
    static member Add ((Vector p), (Vector v)) = Vector (add p v)
    static member Sub ((Vector p), (Vector v)) = Vector (sub p v)
    static member Mul ((Vector p), n) = Vector (mul p n)
    static member Neg (Vector (x, y, z)) = Vector (-x, -y, z)
    static member ToArray (Vector (x, y, z)) = [|x; y; z; wVector|]

type Point = Point of (struct (float * float * float)) with
    static member X (Point (x, _, _)) = x
    static member Y (Point (_, y, _)) = y
    static member Z (Point (_, _, z)) = z
    static member Equal ((Point a), (Point b)) = equal a b
    static member Add ((Point p), (Vector v)) = Point (add p v)
    static member Sub ((Point p), (Point v)) = Vector (sub p v)
    static member Sub ((Point p), (Vector v)) = Point (sub p v)
    static member ToArray (Point (x, y, z)) = [|x; y; z; wPoint|]

type Color = Color of (struct (float * float * float)) with
    static member Equal ((Color a), (Color b)) = equal a b
    static member Add ((Color p), (Color v)) = Color (add p v)
    static member Sub ((Color p), (Color v)) = Color (sub p v)
    static member Mul ((Color p), n) = Color (mul p n)

type Raw = Raw of (struct (float * float * float * float)) with
    static member ToArray (Raw (x, y, z, w)) = [|x; y; z; w|]


// constructors
let point x y z = Point (x, y, z)
let vector x y z = Vector (x, y, z)
let color r g b = Color (r, g, b)
let raw x y z w = Raw (x, y, z, w)


// 'polymorphic' functions
let inline x (prim: ^T) =
    (^T: (static member X: ^T -> float) (prim))
let inline y (prim: ^T) =
    (^T: (static member Y: ^T -> float) (prim))
let inline z (prim: ^T) =
    (^T: (static member Z: ^T -> float) (prim))
let inline (.=) (a: ^T) (b: ^T) =
    (^T: (static member Equal: ^T -> ^T -> bool) (a, b))
let inline (.+) (a: ^T) (b: ^U) =
    (^T: (static member Add: ^T -> ^U -> ^T) (a, b))
let inline (.-) (a: ^T) (b: ^U) =
    (^T: (static member Sub: ^T -> ^U -> ^V) (a, b))
let inline (.*) (a: ^T) (b: ^U) =
    (^T: (static member Mul: ^T -> ^U -> ^T) (a, b))
let inline toArray (a: ^T) =
    (^T: (static member ToArray: ^T -> ^U) (a))


// vector functions
let inline (./) (Vector (x, y, z)) n = vector (x / n) (y / n) (z / n)
let inline neg (Vector (x, y, z)) = vector -x -y -z
let inline mag (Vector (x, y, z)) = (x * x) + (y * y) + (z * z) |> sqrt
let inline norm ((Vector (x, y, z)) as v) =
    let m = mag v in vector (x / m) (y / m) (z / m)
let inline dot (Vector (x, y, z)) (Vector (x', y', z')) =
    (x * x') + (y * y') + (z * z')
let cross (Vector (x, y, z)) (Vector (x', y', z')) =
    vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')


// color functions
let r (Color (r, _, _)) = r
let g (Color (_, g, _)) = g
let b (Color (_, _, b)) = b
let hprod (Color (r, g, b)) (Color (r', g', b')) =
    color (r * r') (g * g') (b * b')

type Primitive =
    | PrimPoint of Point
    | PrimVector of Vector
    | PrimRaw of Raw
    with
    static member ToArray prim =
        match prim with
        | PrimPoint p -> toArray p
        | PrimVector v -> toArray v
        | PrimRaw r -> toArray r

let toPrimitive a =
    match a with
        | [| x; y; z; w |] ->
            match w with
            | 0.0 -> PrimVector (vector x y z)
            | 1.0 -> PrimPoint (point x y z)
            | _ -> PrimRaw (raw x y z w)
        | _ -> failwith "Unexpected length for tuple: ${Array.length a}"
