module Matrix

open Tuple

let matrixOfArrays (arrays: 'a[][]) =
        let height = arrays.Length
        let width = arrays.[0].Length
        if arrays |> Array.forall (Array.length >> ((=) width)) |> not
            then failwith "All arrays must be of same length"
        Array2D.init width height (fun row col ->
            arrays |> Array.item row |> Array.item col)

let matrixOfLists  (lists: 'a list list) =
        lists |> (List.map List.toArray) |> List.toArray |> matrixOfArrays

let (|*|) a b =
    let height, width = Array2D.length1 a, Array2D.length2 a
    if width <> Array2D.length1 b
        then failwith "Matrices are wrong shape for multiplication"
    Array2D.init height (Array2D.length2 b) (fun row col ->
        (a.[row, *], b.[*, col]) ||> Array.map2 (*) |> Array.reduce (+))

let (|*) A b =
    let B = Array2D.zeroCreate 3 1
    B.[0, 0] <- x b
    B.[1, 0] <- y b
    B.[2, 0] <- z b
    let R = A |*| B
    vector R.[0, 0] R.[1, 0] R.[2, 0]

let identity n =
    let M = Array2D.zeroCreate n n
    [0..n-1] |> List.iter (fun n -> M.[n, n] <- 1.0)
    M

let identityMatrix =
#if TYPED
    let size = 3
#else
    let size = 4
#endif
    identity size

let inline transpose M =
    Array2D.init
        (Array2D.length2 M)
        (Array2D.length1 M)
        (fun x y -> M.[y, x])

let rec determinant M =
    let height, width = Array2D.length1 M, Array2D.length2 M
    if height <> width then
        failwith "Can only get determinant of a square matrix"
    match width with
    | 2 -> 0.0 + M.[0, 0] * M.[1, 1] - M.[0, 1] * M.[1, 0]
    | _ -> [0..width-1] |> List.sumBy(fun c -> M.[0, c] * cofactor M 0 c)

and submatrix parent r c =
    let height, width = Array2D.length1 parent, Array2D.length2 parent
    let subHght, subWdth = height - 1, width - 1
    let sub = Array2D.zeroCreate subHght subWdth
    Array2D.blit parent 0 0 sub 0 0 r c
    Array2D.blit parent 0 (c + 1) sub 0 c r (subWdth - c)
    Array2D.blit parent (r + 1) 0 sub r 0 (subHght - r) c
    Array2D.blit parent (r + 1) (c + 1) sub r c (subHght - r) (subWdth - c)
    sub

and minor A r c = submatrix A r c |> determinant

and cofactor A r c =
    let minor = minor A r c
    match (r + c) % 2 with
    | 0 -> minor
    | _ -> -minor

let invertible = determinant >> ((<>) 0.0)

let inverse M =
    M
    |> Array2D.mapi (fun r c _ -> cofactor M r c)
    |> transpose
    |> Array2D.map (fun n -> n / (determinant M))
