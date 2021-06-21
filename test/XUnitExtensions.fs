module XUnitExtensions

open Xunit

open Primitives

type TracerAssert () =
    inherit Assert ()

    // static member Equal (a: Point, b: Point) =
    //     if  (a <> b) then raise (Sdk.EqualException(a, b))

    // static member Equal (a: Vector, b: Vector) =
    //     if  (a <> b) then raise (Sdk.EqualException(a, b))

    // static member Equal (a: Color, b: Color) =
    //     if  (a <> b) then raise (Sdk.EqualException(a, b))

    static member Equal (a: float[], b: float[]) =
        if  (a <> b) then raise (Sdk.EqualException(a, b))

    static member ValEqual (a, b) =
        if not (valEqual a b) then raise (Sdk.EqualException(a, b))
