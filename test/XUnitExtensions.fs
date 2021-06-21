module XUnitExtensions

open Xunit

open Primitives

type TracerAssert () =
    inherit Assert ()

    static member Equal (a: Point, b: Point) =
        if not (a .= b) then
            raise (Sdk.EqualException(a, b))

    static member Equal (a: Vector, b: Vector) =
        if not (a .= b) then
            raise (Sdk.EqualException(a, b))

    static member Equal (a: Color, b: Color) =
        if not (a .= b) then
            raise (Sdk.EqualException(a, b))

    static member ValEqual (a, b) =
        if not (valEqual a b) then
            raise (Sdk.EqualException(a, b))
