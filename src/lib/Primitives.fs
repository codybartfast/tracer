﻿module Primitives

open System

// for untyped representations of a primitive type
type Bare = float[]
type Prim =
    static member Bare ([<ParamArray>] arr : 'a array) = arr

let epsilon = 0.00001
let wPoint = 1.0
let wVector = 0.0

(* tuple functions *)
let valEqual a b = a - b |> abs |> (>) epsilon
let inline private equal (struct(x, y, z)) (struct(x', y', z')) =
    let valEqual a b = a - b |> abs |> (>) epsilon
    (valEqual x x')
    && (valEqual y y')
    && (valEqual z z')
let inline private add (struct(x, y, z)) (struct(x', y', z')) =
    (struct(x + x', y + y', z + z'))
let inline private sub (struct(x, y, z)) (struct(x', y', z')) =
    (struct(x - x', y - y', z - z'))
let inline private mul (struct(x, y, z)) n = struct (x * n, y * n, z * n)
let inline private div (struct(x, y, z)) n = struct (x / n, y / n, z / n)


(* primitive types *)
[<CustomEquality>][<NoComparison>]
type Vector = Vector of (struct (float * float * float)) with
    static member X (Vector (x, _, _)) = x
    static member Y (Vector (_, y, _)) = y
    static member Z (Vector (_, _, z)) = z
    static member (+) ((Vector p), (Vector v)) = Vector (add p v)
    static member (-) ((Vector p), (Vector v)) = Vector (sub p v)
    static member (~-) (Vector (x, y, z)) = Vector (-x, -y, -z)
    static member (*) ((Vector p), n) = Vector (mul p n)
    static member (/) ((Vector p), n) = Vector (div p n)
    static member ToBare (Vector (x, y, z)) = [|x; y; z; wVector|]
    override a.Equals b =
        match b with
        | :? Vector as b ->
            let (Vector a) = a
            let (Vector b) = b
            equal a b
        | _ -> false
    override _.GetHashCode () = 0

// vector functions
let inline vector x y z = Vector (x, y, z)
let inline toVector (b: Bare) =
    match b with
    | [| x; y; z; w |] when (valEqual w wVector) -> vector x y z
    | _ -> failwith $"Unexpected length for bare point: ${Array.length b}"
let inline mag (Vector (x, y, z)) = (x * x) + (y * y) + (z * z) |> sqrt
let inline norm ((Vector (x, y, z)) as v) =
    let m = mag v in vector (x / m) (y / m) (z / m)
let inline dot (Vector (x, y, z)) (Vector (x', y', z')) =
    (x * x') + (y * y') + (z * z')
let inline cross (Vector (x, y, z)) (Vector (x', y', z')) =
    vector (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')

[<CustomEquality>][<NoComparison>]
type Point = Point of (struct (float * float * float)) with
    static member X (Point (x, _, _)) = x
    static member Y (Point (_, y, _)) = y
    static member Z (Point (_, _, z)) = z
    static member Equals ((Point a), (Point b)) = equal a b
    static member (+) ((Point p), (Vector v)) = Point (add p v)
    static member (-) ((Point p), (Point v)) = Vector (sub p v)
    static member (-) ((Point p), (Vector v)) = Point (sub p v)
    static member ToBare (Point (x, y, z)) = [|x; y; z; wPoint|]
    override a.Equals b =
        match b with
        | :? Point as b ->
            let (Point a) = a
            let (Point b) = b
            equal a b
        | _ -> false
    override _.GetHashCode () = 0

// point functions
let inline point x y z = Point (x, y, z)
let inline toPoint (b: Bare) =
    match b with
    | [| x; y; z; w |] when (valEqual w wPoint) -> point x y z
    | _ -> failwith $"Unexpected length for bare point: ${Array.length b}"

[<CustomEquality>][<NoComparison>]
type Color = Color of (struct (float * float * float)) with
    static member Equals ((Color a), (Color b)) = equal a b
    static member (+) ((Color p), (Color v)) = Color (add p v)
    static member (-) ((Color p), (Color v)) = Color (sub p v)
    static member (*) ((Color p), n) = Color (mul p n)
    override a.Equals b =
        match b with
        | :? Color as b ->
            let (Color a) = a
            let (Color b) = b
            equal a b
        | _ -> false
    override _.GetHashCode () = 0

// color functions
let inline color r g b = Color (r, g, b)
let inline r (Color (r, _, _)) = r
let inline g (Color (_, g, _)) = g
let inline b (Color (_, _, b)) = b
let inline hprod (Color (r, g, b)) (Color (r', g', b')) =
    color (r * r') (g * g') (b * b')

type Exotic = Exotic of (struct (float * float * float * float)) with
    static member ToBare (Exotic (x, y, z, w)) = [|x; y; z; w|]
let exotic x y z w = Exotic (x, y, z, w)


// 'polymorphic' functions
let inline x (prim: ^T) =
    (^T: (static member X: ^T -> float) (prim))
let inline y (prim: ^T) =
    (^T: (static member Y: ^T -> float) (prim))
let inline z (prim: ^T) =
    (^T: (static member Z: ^T -> float) (prim))
let inline toBare (a: ^T) =
    (^T: (static member ToBare: ^T -> Bare) (a))
