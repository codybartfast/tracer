module Primitives

let private epsilon = 0.00001
let private wPoint = 1.0
let private wVector = 0.0

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

type Point = Point of (struct (float * float * float)) with
    static member X (Point (x, _, _)) = x
    static member Y (Point (_, y, _)) = y
    static member Z (Point (_, _, z)) = z
    static member Equal ((Point a), (Point b)) = equal a b
    static member Add ((Point p), (Vector v)) = Point (add p v)
    static member Sub ((Point p), (Point v)) = Vector (sub p v)
    static member Sub ((Point p), (Vector v)) = Point (sub p v)

and Vector = Vector of (struct (float * float * float)) with
    static member X (Vector (x, _, _)) = x
    static member Y (Vector (_, y, _)) = y
    static member Z (Vector (_, _, z)) = z
    static member Equal ((Vector a), (Vector b)) = equal a b
    static member Add ((Vector p), (Vector v)) = Vector (add p v)
    static member Sub ((Vector p), (Vector v)) = Vector (sub p v)
    static member Neg (Vector (x, y, z)) = Vector (-x, -y, z)

let point x y z = Point (x, y, z)
let vector x y z = Vector (x, y, z)

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

let inline (.*) (Vector (x, y, z)) n = vector (x * n) (y * n) (z * n)
let inline (./) (Vector (x, y, z)) n = vector (x / n) (y / n) (z / n)
let inline neg (Vector (x, y, z)) = vector -x -y -z
let inline mag (Vector (x, y, z)) = (x * x) + (y * y) + (z * z) |> sqrt
let inline norm ((Vector (x, y, z)) as v) =
    let m = mag v in vector (x / m) (y / m) (z / m)
let inline dot (Vector (x, y, z)) (Vector (x', y', z')) =
    (x * x') + (y * y') + (z * z')
let cross (Vector (x, y, z)) (Vector (x', y', z')) =
    vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

// let hprod a b =
//     match a, b with
//     | Color (ar, ag, ab), Color (br, bg, bb) ->
//         color (ar * br) (ag * bg) (ab * bb)
//     | _ -> failwith "habamard only accepts colors"

// let toArray t =
//     match t with
//     | Exotic a -> a
//     | Point _ -> [| x t; y t; z t; wPoint |]
//     | Vector _ -> [| x t; y t; z t; wVector |]
//     | Color _ -> failwith "Can't get matrix from colour"

// let toTuple a =
//     match a with
//         | [| x; y; z; w |] ->
//             match w with
//             | 0.0 -> vector x y z
//             | 1.0 -> point x y z
//             | _ -> exotic x y z w
//         | _ -> failwith "Unexpected length for tuple: ${Array.length a}"
