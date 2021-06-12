// #if !Typed

module Tuple

let private epsilon = 0.00001
let private wPoint = 1.0
let private wVector = 0.0
let private wColor = 0.0


// (* Tuple Struct implementation - not compatible with fsi *)
// type Tuple = (struct (float * float * float * float))
// let rawTuple  x y z w : Tuple = (x, y, z, w)
// let x ((x, _, _, _): Tuple)  = x
// let y ((_, y, _, _): Tuple)  = y
// let z ((_, _, z, _): Tuple)  = z
// let w ((_, _, _, w): Tuple)  = w

(* Typed Tuple Struct implementation *)
type Tuple = Tuple of (struct (float * float * float * float))
let rawTuple  x y z w : Tuple = Tuple (x, y, z, w)
let x (Tuple (x, _, _, _))  = x
let y (Tuple (_, y, _, _))  = y
let z (Tuple (_, _, z, _))  = z
let w (Tuple (_, _, _, w))  = w


// (* Record Struct implementation - not compatible with fsi *)
// [<Struct>]
// type Tuple = { x: float; y: float; z: float; W: float }
// let rawTuple x y z w : Tuple = { x = x; y = y; z = z; W = w }
// let x t = t.x
// let y t = t.y
// let z t = t.z
// let w t = t.W

// (* Typed Record Struct implementation *)
// [<Struct>]
// type TupleRecord = { x: float; y: float; z: float; W: float }
// type Tuple = Tuple of TupleRecord
// let rawTuple x y z w : Tuple = Tuple { x = x; y = y; z = z; W = w }
// let x (Tuple t) = t.x
// let y (Tuple t) = t.y
// let z (Tuple t) = t.z
// let w (Tuple t) = t.W


(* Common implementation *)
let point x y z = rawTuple x y z wPoint
let isPoint t = w t = wPoint

let vector x y z = rawTuple x y z wVector
let isVector t = w t = wVector

let color r g b = rawTuple r g b wColor
let red = x   // why not 'red = x'?
let green = y
let blue = z

let exotic x y z w = rawTuple x y z w

let valEqual a b = a - b |> abs |> (>) epsilon
let equal a b =
    valEqual (x a) (x b)
    && valEqual (y a) (y b)
    && valEqual (z a) (z b)
    && valEqual (w a) (w b)

let add a b = rawTuple (x a + x b) (y a + y b) (z a + z b) (w a + w b)
let (.+) = add
let sub a b = rawTuple (x a - x b) (y a - y b) (z a - z b) (w a - w b)
let (.-) = sub
let neg a = rawTuple -(x a) -(y a) -(z a) -(w a)
let mul a s = rawTuple (x a * s) (y a * s) (z a * s) (w a * s)
let (.*) = mul
let div a s = rawTuple (x a / s) (y a / s) (z a / s) (w a / s)
let (./) = div
let mag t =
    let x, y, z = x t, y t, z t
    (x * x) + (y * y) + (z * z)
    |> sqrt
let norm t =
    let m = mag t
    vector (x t / m) (y t / m) (z t / m)
let dot a b = (x a * x b) + (y a * y b) + (z a * z b) + (w a * w b)
let cross a b =
    let ax, ay, az = x a, y a, z a
    let bx, by, bz = x b, y b, z b
    vector (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)
let hprod a b = rawTuple (red a * red b) (green a * green b) (blue a * blue b) wColor

let toArray t = [| x t; y t; z t; w t |]

let toTuple a =
    match a with
        | [| x; y; z; w |] ->
            match w with
            | 0.0 -> vector x y z
            | 1.0 -> point x y z
            | _ -> exotic x y z w
        | _ -> failwith "Unexpected length for tuple: ${Array.length a}"