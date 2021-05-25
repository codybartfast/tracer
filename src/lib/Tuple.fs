module Tuple

module Tuple =
    let EPSILON = 0.00001
    let private wPoint = 1.0
    let private wVector = 0.0


    (* Tuple implementation *)
    // type Tuple = (struct (float * float * float * float))
    // let raw  x y z w : Tuple = (x, y, z, w)
    // let X ((x, _, _, _): Tuple)  = x
    // let Y ((_, y, _, _): Tuple)  = y
    // let Z ((_, _, z, _): Tuple)  = z
    // let W ((_, _, _, w): Tuple)  = w
    // let isPoint t = W t = wPoint
    // let isVector t = W t = wVector


    (* Struct Record implementation *)
    [<Struct>]
    type Tuple =
        { X: float
          Y: float
          Z: float
          W: float }
    let raw x y z w : Tuple = { X = x; Y = y; Z = z; W = w }
    let X t = t.X
    let Y t = t.Y
    let Z t = t.Z
    let W t = t.W
    let isPoint t = W t = wPoint
    let isVector t = W t = wVector


    (* Common implementation *)
    let point x y z = raw x y z wPoint
    let vector x y z = raw x y z wVector

    let floatEqual a b = a - b |> abs |> (>) EPSILON

    let equal a b =
        floatEqual (X a) (X b)
        && floatEqual (Y a) (Y b)
        && floatEqual (Z a) (Z b)
        && floatEqual (W a) (W b)

    let add a b = raw (X a + X b) (Y a + Y b) (Z a + Z b) (W a + W b)
    let sub a b = raw (X a - X b) (Y a - Y b) (Z a - Z b) (W a - W b)
    let neg a = raw -(X a) -(Y a) -(Z a) -(W a)
    let mul a s = raw (X a * s) (Y a * s) (Z a * s) (W a * s)
    let div a s = raw (X a / s) (Y a / s) (Z a / s) (W a / s)
    let mag t = 
        let x, y, z = X t, Y t, Z t
        (x * x) + (y * y) + (z * z)
        |> sqrt
    let norm t =
        let m = mag t
        vector (X t / m) (Y t / m) (Z t / m)
    let dot a b = 
        (X a * X b) + (Y a * Y b) + (Z a * Z b) + (W a * W b)
    let cross a b = 
        let ax, ay, az = X a, Y a, Z a
        let bx, by, bz = X b, Y b, Z b
        vector (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

type Tuple = Tuple.Tuple
