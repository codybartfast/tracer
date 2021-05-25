module Tuple

let private epsilon = 0.00001
let private wPoint = 1.0
let private wVector = 0.0


(* Tuple Struct implementation *)
// type Struct Tuple = (struct (float * float * float * float))
// let rawTuple  x y z w : Tuple = (x, y, z, w)
// let X ((x, _, _, _): Tuple)  = x
// let Y ((_, y, _, _): Tuple)  = y
// let Z ((_, _, z, _): Tuple)  = z
// let W ((_, _, _, w): Tuple)  = w

(* Typed Tuple Struct implementation *)
type Tuple = Tuple of (struct (float * float * float * float))
let rawTuple  x y z w : Tuple = Tuple (x, y, z, w)
let X (Tuple (x, _, _, _))  = x
let Y (Tuple (_, y, _, _))  = y
let Z (Tuple (_, _, z, _))  = z
let W (Tuple (_, _, _, w))  = w


(* Record Struct implementation *)
// [<Struct>]
// type Tuple = { X: float; Y: float; Z: float; W: float }
// let rawTuple x y z w : Tuple = { X = x; Y = y; Z = z; W = w }
// let X t = t.X
// let Y t = t.Y
// let Z t = t.Z
// let W t = t.W

(* Typed Record Struct implementation *)
// [<Struct>]
// type TupleRecord = { X: float; Y: float; Z: float; W: float }
// type Tuple = Tuple of TupleRecord
// let rawTuple x y z w : Tuple = Tuple { X = x; Y = y; Z = z; W = w }
// let X (Tuple t) = t.X
// let Y (Tuple t) = t.Y
// let Z (Tuple t) = t.Z
// let W (Tuple t) = t.W


(* Common implementation *)
let point x y z = rawTuple x y z wPoint
let isPoint t = W t = wPoint

let vector x y z = rawTuple x y z wVector
let isVector t = W t = wVector

let valEqual a b = a - b |> abs |> (>) epsilon
let equal a b =
    valEqual (X a) (X b)
    && valEqual (Y a) (Y b)
    && valEqual (Z a) (Z b)
    && valEqual (W a) (W b)

let add a b = rawTuple (X a + X b) (Y a + Y b) (Z a + Z b) (W a + W b)
let (.+) = add
let sub a b = rawTuple (X a - X b) (Y a - Y b) (Z a - Z b) (W a - W b)
let (.-) = sub
let neg a = rawTuple -(X a) -(Y a) -(Z a) -(W a)
let mul a s = rawTuple (X a * s) (Y a * s) (Z a * s) (W a * s)
let (.*) = mul
let div a s = rawTuple (X a / s) (Y a / s) (Z a / s) (W a / s)
let (./) = div
let mag t =
    let x, y, z = X t, Y t, Z t
    (x * x) + (y * y) + (z * z)
    |> sqrt
let norm t =
    let m = mag t
    vector (X t / m) (Y t / m) (Z t / m)
let dot a b = (X a * X b) + (Y a * Y b) + (Z a * Z b) + (W a * W b)
let cross a b =
    let ax, ay, az = X a, Y a, Z a
    let bx, by, bz = X b, Y b, Z b
    vector (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)
