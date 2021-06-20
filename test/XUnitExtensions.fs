module XUnitExtensions

open Xunit

open Primitives

type TracerAssert () =
    inherit Assert ()

    static member PointEqual (a: Point, b: Point) =
        if not (a .= b) then
            raise (Sdk.EqualException(a, b))

    static member VectorEqual (a: Vector, b: Vector) =
        if not (a .= b) then
            raise (Sdk.EqualException(a, b))

    static member ValEqual (a, b) =
        if not (valEqual a b) then
            raise (Sdk.EqualException(a, b))
