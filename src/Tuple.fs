module Tuple

module Tuple =
    let EPSILON = 0.00001
    let private wPoint = 1.0
    let private wVector = 0.0


    (* Tuple implementation *)
    type Tuple = float * float * float * float
    let raw  x y z w : Tuple = (x, y, z, w)
    let X (x, _, _, _) = x
    let Y (_, y, _, _) = y
    let Z (_, _, z, _) = z
    let W (_, _, _, w) = w
    let isPoint t = W t = wPoint
    let isVector t = W t = wVector


    (* Struct Record implementation *)
    // [<Struct>]
    // type Tuple =
    //     { X: float
    //       Y: float
    //       Z: float
    //       W: float }

    // let raw x y z w : Tuple = { X = x; Y = y; Z = z; W = w }
    // let X t = t.X
    // let Y t = t.Y
    // let Z t = t.Z
    // let W t = t.W
    // let isPoint t = W t = wPoint
    // let isVector t = W t = wVector


    (* Common implementation *)
    let point x y z = raw x y z wPoint
    let vector x y z = raw x y z wVector

    let private floatEqual a b = a - b |> abs |> (>) EPSILON

    let private equal a b =
        floatEqual (X a) (X b)
        && floatEqual (Y a) (Y b)
        && floatEqual (Z a) (Z b)
        && floatEqual (W a) (W b)
