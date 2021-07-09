module XUnitExtensions

open Xunit

open Primitives
open Shapes

type TracerAssert () =
    inherit Assert ()

    // static member Equal (a: Point, b: Point) =
    //     if  (a <> b) then raise (Sdk.EqualException(a, b))

    // static member Equal (a: Vector, b: Vector) =
    //     if  (a <> b) then raise (Sdk.EqualException(a, b))

    // static member Equal (a: Color, b: Color) =
    //     if  (a <> b) then raise (Sdk.EqualException(a, b))

    static member Equal (a: PointLight list, b: PointLight list) =
        if  (a <> b) then raise (Sdk.EqualException(a, b))

    static member Equal (a: float[], b: float[]) =
        if  (a <> b) then raise (Sdk.EqualException(a, b))

    static member Equal (a: float, b: float) =
        if not (valEqual a b) then raise (Sdk.EqualException(a, b))

    static member Equal (a: Shape, b: Shape) =
        if  (a <> b) then raise (Sdk.EqualException(a, b))
