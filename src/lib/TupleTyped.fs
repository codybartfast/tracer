module Tuple

let private epsilon = 0.00001
let private wPoint = 1.0
let private wVector = 0.0

type RawTuple = (struct (float * float * float))

type Tuple =
    | Color of RawTuple
    | Point of RawTuple
    | Vector of RawTuple
    | Exotic of float[]

let (|RawTuple|) = function
    | Color rt -> rt
    | Point rt -> rt
    | Vector rt -> rt
    | Exotic _ -> failwith "I don't handle the unknown"

let x (RawTuple (x, _, _)) = x
let y (RawTuple (_, y, _)) = y
let z (RawTuple (_, _, z)) = z

let rawTuple x y z = RawTuple(x, y, z)
let tuple hint rt =
    match hint with
    | Color _ -> Color rt
    | Point _ -> Point rt
    | Vector _ -> Vector rt
    | Exotic _ -> failwith "I don't like the unknown"

let point x y z = Point(rawTuple x y z)
let isPoint = function
    | (Point _) -> true
    | _ -> false

let vector x y z = Vector(rawTuple x y z)
let isVector = function
    | (Vector _) -> true
    | _ -> false

let color r g b = Color(rawTuple r g b)
let r = x
let g = y
let b = z

let exotic x y z w = Exotic [| x; y; z; w |]

let valEqual a b = a - b |> abs |> (>) epsilon
let typeEqual a b =
    match a, b with
    | (Color _), (Color _) -> true
    | (Point _), (Point _) -> true
    | (Vector _), (Vector _) -> true
    | _ -> false
let equal a b =
    typeEqual a b
    && valEqual (x a) (x b)
    && valEqual (y a) (y b)
    && valEqual (z a) (z b)

let add a b =
    let rt =
        rawTuple (x a + x b) (y a + y b) (z a + z b)
    match a, b with
    | Color _, Color _ -> Color rt
    | Vector _, Vector _ -> Vector rt
    | Point _, Vector _ -> Point rt
    | _ -> failwith "Can't add given types."
let (.+) = add

let sub a b =
    let rt = rawTuple (x a - x b) (y a - y b) (z a - z b)
    match a, b with
    | Color _, Color _ -> Color rt
    | Point _, Point _ -> Vector rt
    | Point _, Vector _ -> Point rt
    | Vector _, Vector _  -> Vector rt
    | _ -> failwith "Can't subtract given types"
let (.-) = sub

let neg a = rawTuple -(x a) -(y a) -(z a) |> tuple a

let mul a s = rawTuple (x a * s) (y a * s) (z a * s) |> tuple a
let (.*) = mul

let div a s = rawTuple (x a / s) (y a / s) (z a / s) |> tuple a
let (./) = div

let mag a =
    let x, y, z = x a, y a, z a
    (x * x) + (y * y) + (z * z) |> sqrt

let norm v =
    let m = mag v
    match v with
    | (Vector _) -> vector (x v / m) (y v / m) (z v / m)
    | _ -> failwith "norm only accepts vectors"

let dot a b =
    match a, b with
    | (Vector _), (Vector _) -> (x a * x b) + (y a * y b) + (z a * z b)
    | _ -> failwith "dot only accepts vectors"

let cross a b =
    let ax, ay, az = x a, y a, z a
    let bx, by, bz = x b, y b, z b
    match a, b with
    | (Vector _), (Vector _) ->
        vector (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)
    | _ -> failwith "cross only accepts vectors"

let hprod a b =
    match a, b with
    | Color (ar, ag, ab), Color (br, bg, bb) ->
        color (ar * br) (ag * bg) (ab * bb)
    | _ -> failwith "habamard only accepts colors"

let toArray t =
    match t with
    | Exotic a -> a
    | Point _ -> [| x t; y t; z t; wPoint |]
    | Vector _ -> [| x t; y t; z t; wVector |]
    | Color _ -> failwith "Can't get matrix from colour"

let toTuple a =
    match a with
        | [| x; y; z; w |] ->
            match w with
            | 0.0 -> vector x y z
            | 1.0 -> point x y z
            | _ -> exotic x y z w
        | _ -> failwith "Unexpected length for tuple: ${Array.length a}"
