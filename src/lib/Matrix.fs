module Matrix

open Primitives

type Matrix (data: float[,]) =

    let transpose m =
        Array2D.init
            (Array2D.length2 m)
            (Array2D.length1 m)
            (fun x y -> m.[y, x])

    let rec determinant m =
        let height, width = Array2D.length1 m, Array2D.length2 m
        if height <> width then
            failwith "Can only get determinant of a square matrix"
        match width with
        | 2 -> 0.0 + m.[0, 0] * m.[1, 1] - m.[0, 1] * m.[1, 0]
        | _ -> [0..width-1] |> List.sumBy(fun c -> m.[0, c] * cofactor m 0 c)

    and submatrix parent r c =
        let height, width = Array2D.length1 parent, Array2D.length2 parent
        let subHght, subWdth = height - 1, width - 1
        let sub = Array2D.zeroCreate subHght subWdth
        Array2D.blit parent 0 0 sub 0 0 r c
        Array2D.blit parent 0 (c + 1) sub 0 c r (subWdth - c)
        Array2D.blit parent (r + 1) 0 sub r 0 (subHght - r) c
        Array2D.blit parent (r + 1) (c + 1) sub r c (subHght - r) (subWdth - c)
        sub

    and minor m r c = submatrix m r c |> determinant

    and cofactor m r c =
        let minor = minor m r c
        match (r + c) % 2 with
        | 0 -> minor
        | _ -> -minor

    let inverse m =
        let detm = determinant m
        m
        |> Array2D.mapi (fun r c _ -> cofactor m r c)
        |> transpose
        |> Array2D.map (fun n -> n / detm)

    member private _.Data = data

    new (height: int, width: int, init: int -> int -> float) =
        Matrix(Array2D.init height width init)

    new (rows: float[][]) =
        let height = rows.Length
        let width = rows.[0].Length
        if rows |> Array.forall (Array.length >> ((=) width)) |> not
            then failwith "All arrays must be of same length"
        Matrix(height, width, (fun row col ->
            rows |> Array.item row |> Array.item col))

    new (rows: float list list) =
        let rows = rows |> (List.map List.toArray) |> List.toArray
        Matrix rows

    new (rows: int list list) =
        let rows = List.map (List.map float) rows
        Matrix rows

    new (arr: float[]) = Matrix( arr.Length, 1, (fun r c -> arr.[r]))

    member _.Item
        with get(r, c) = data.[r, c]
        and set(r, c) v = data.[r, c] <- v

    member _.Col
        with get(c) = data.[*, c]

    static member ( * ) (m: Matrix, n: Matrix) =
        let m, n = m.Data, n.Data
        let height, width = Array2D.length1 m, Array2D.length2 m
        let height', width' = Array2D.length1 n, Array2D.length2 n
        if width <> height' then failwith "Matrices are wrong shape for (*)"
        Matrix(
            height,
            width',
            fun row col -> Array.map2 (*) m.[row, *] n.[*, col] |> Array.sum)

    static member ( * ) (m: Matrix, bare: Bare) =
        let m = m.Data
        Array.init
            (Array2D.length1 m)
            (fun i -> Array.map2 (*) m.[i, *] bare |> Array.sum)

    static member ( * ) (m: Matrix, p: Point) = (m * (toBare p))
    static member ( * ) (m: Matrix, v: Vector) = (m * (toBare v))
    static member ( * ) (m: Matrix, e: Exotic) = (m * (toBare e))
    static member ( .* ) (m: Matrix, p: Point) = (m * p |> toPoint)
    static member ( .* ) (m: Matrix, v: Vector) = (m * v |> toVector)

    override _.Equals b =
        match b with
        | :? Matrix as b ->
            data =  b.Data
        | _ -> false
    override _.GetHashCode() = data.GetHashCode()

    member _.Transpose() = Matrix(transpose data)
    member _.Determinant() = determinant data
    member _.Submatrix(r, c) = Matrix (submatrix data r c)
    member _.Minor(r, c) = minor data r c
    member _.Cofactor(r, c) = cofactor data r c
    member _.Invertible() = data |> determinant |> ((<>) 0.0)
    member _.Inverse() = Matrix(inverse data)
    member _.Map(mapper) = Matrix(Array2D.map mapper data)

let transpose (m: Matrix) = m.Transpose()
let determinant (m: Matrix) = m.Determinant()
let submatrix (m: Matrix) r c = m.Submatrix(r, c)
let minor (m: Matrix) r c = m.Minor(r, c)
let cofactor (m: Matrix) r c = m.Cofactor(r, c)
let invertible (m: Matrix) = m.Invertible()
let inverse (m: Matrix) = m.Inverse()

let identityN size = Matrix (size, size, fun r c -> if r = c then 1.0 else 0.0)
let identity () = identityN 4
let zeroMatrixN size = Array2D.zeroCreate size size |> Matrix
let zeroMatrix () = zeroMatrixN 4

let inline ( |* ) b a = a * b
let inline ( |.* ) b a = a .* b
