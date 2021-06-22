module MatrixTests

open System
open Xunit

open Matrix
open Primitives

type Assert = XUnitExtensions.TracerAssert

let round (m: Matrix)= m.Map (fun (n: float) -> Math.Round(n, 5))

[<Theory>]
[<InlineData(1.0, 0, 0)>]
[<InlineData(4.0, 0, 3)>]
[<InlineData(5.5, 1, 0)>]
[<InlineData(7.5, 1, 2)>]
[<InlineData(11.0, 2, 2)>]
[<InlineData(13.5, 3, 0)>]
[<InlineData(15.5, 3, 2)>]
let ``constructing and inspecting a 4x4 matrix`` (expected, x, y) =
    let m =
        Matrix [ [ 1.0; 2.0; 3.0; 4.0 ]
                 [ 5.5; 6.5; 7.5; 8.5 ]
                 [ 9.0; 10.0; 11.0; 12.0 ]
                 [ 13.5; 14.5; 15.5; 16.5 ] ]
    Assert.Equal(expected, m.[x, y])

[<Theory>]
[<InlineData(-3, 0, 0)>]
[<InlineData(5, 0, 1)>]
[<InlineData(1, 1, 0)>]
[<InlineData(-2, 1, 1)>]
let ``a 2x2 matrix ought to be representable`` (expected, x, y) =
    let m = Matrix [ [ -3; 5 ]
                     [ 1; -2 ] ]
    Assert.Equal(expected, m.[x, y])

[<Theory>]
[<InlineData(-3, 0, 0)>]
[<InlineData(-2, 1, 1)>]
[<InlineData(1, 2, 2)>]
let ``a 3x3 matrix ought to be representable`` (expected, x, y) =
    let m = Matrix [ [ -3; 5; 0 ]
                     [ 1; -2; -7 ]
                     [ 0; 1; 1] ]
    Assert.Equal(expected, m.[x, y])

[<Fact>]
let ``matrix equality with identical matrices`` () =
    let a = Matrix [ [1; 2; 3; 4]
                     [5; 6; 7; 8]
                     [9; 8; 7; 6]
                     [5; 4; 3; 2] ]
    let b = Matrix [ [1; 2; 3; 4]
                     [5; 6; 7; 8]
                     [9; 8; 7; 6]
                     [5; 4; 3; 2] ]
    Assert.True( (a = b) )

[<Fact>]
let ``matrix equality with different matrices`` () =
    let a = Matrix [ [1; 2; 3; 4]
                     [5; 6; 7; 8]
                     [9; 8; 7; 6]
                     [5; 4; 3; 2] ]
    let b = Matrix [ [2; 3; 4; 5]
                     [6; 7; 8; 9]
                     [8; 7; 6; 5]
                     [4; 3; 2; 1] ]
    Assert.True( (a <> b) )

[<Fact>]
let ``multiplying two matrices`` () =
    let a = Matrix [ [1.0; 2.0; 3.0; 4.0]
                     [5.0; 6.0; 7.0; 8.0]
                     [9.0; 8.0; 7.0; 6.0]
                     [5.0; 4.0; 3.0; 2.0] ]
    let b = Matrix [ [-2.0; 1.0; 2.0; 3.0]
                     [3.0; 2.0; 1.0; -1.0]
                     [4.0; 3.0; 6.0; 5.0]
                     [1.0; 2.0; 7.0; 8.0] ]
    let actual = a * b
    let expected = Matrix [ [20.0; 22.0; 50.0; 48.0]
                            [44.0; 54.0; 114.0; 108.0]
                            [40.0; 58.0; 110.0; 102.0]
                            [16.0; 26.0; 46.0; 42.0] ]
    Assert.True((actual = expected))


[<Fact>]
let ``a matrix multiplied by a tuple`` () =
    let m = Matrix [ [1; 2; 3; 4]
                     [2; 4; 4; 2]
                     [8; 6; 4; 1]
                     [0; 0; 0; 1] ]
    let b = point 1.0 2.0 3.0
    let expected = [| 18.0; 24.0; 33.0; 1.0 |]
    let actual = m * b
    Assert.Equal(expected, actual)

[<Fact>]
let ``multiplying a matrix by identity matrix`` () =
    let m = Matrix [ [0.0; 1.0; 2.0; 4.0]
                     [1.0; 2.0; 4.0; 8.0]
                     [2.0; 4.0; 8.0; 16.0]
                     [4.0; 8.0; 16.0; 32.0] ]
    Assert.Equal(m, m * identity ())

[<Fact>]
let ``multiplying the identity matrix by a tuple`` () =
    let a = exotic 1.0 2.0 3.0 4.0
    let actual = identity () * a
    Assert.Equal(toBare a, actual)

[<Fact>]
let ``transposing a matrix`` () =
    let m  = Matrix [ [0; 9; 3; 0]
                      [9; 8; 0; 8]
                      [1; 8; 5; 3]
                      [0; 0; 5; 8] ]
    let expected  = Matrix [ [0; 9; 1; 0]
                             [9; 8; 8; 0]
                             [3; 0; 5; 5]
                             [0; 8; 3; 8] ]
    Assert.Equal(expected, transpose m)
    Assert.Equal(m, m |> transpose |> transpose)

[<Fact>]
let ``transpose the identitymatrix`` () =
    Assert.Equal(identity (), identity () |> transpose)

[<Fact>]
let ``calculating the determinant of a 2x2 matrix`` () =
    let m = Matrix [ [1; 5]
                     [-3; 2]]
    Assert.Equal(17.0, determinant m)

[<Fact>]
let ``a submatrix of a 3x3 matrix is a 2x2 matrix`` () =
    let m = Matrix [ [1; 5; 0]
                     [-3; 2; 7]
                     [0; 6; -3] ]
    let expected = Matrix [ [-3; 2]
                            [0; 6] ]
    Assert.Equal(expected, submatrix m 0 2)

[<Fact>]
let ``a submatrix of a 4x4 matrix is a 3x3 matrix`` () =
    let m = Matrix [ [-6; 1; 1; 6]
                     [-8; 5; 8; 6]
                     [-1; 0; 8; 1]
                     [-7; 1; -1; 1] ]
    let expected = Matrix [ [-6; 1; 6]
                            [-8; 8; 6]
                            [-7; -1; 1] ]
    Assert.Equal(expected, submatrix m 2 1)

[<Fact>]
let ``calculating a minor of a 3x3 matrix`` () =
    let m = Matrix [ [3; 5; 0]
                     [2; -1; -7]
                     [6; -1; 5] ]
    let n = submatrix m 1 0
    Assert.Equal(25.0, determinant n)
    Assert.Equal(25.0, minor m 1 0)

[<Fact>]
let ``calculating a cofactor of a 3x3 matrix`` () =
    let m = Matrix [ [3; 5; 0]
                     [2; -1; -7]
                     [6; -1; 5] ]
    Assert.Equal(-12.0, minor m 0 0)
    Assert.Equal(-12.0, cofactor m 0 0)
    Assert.Equal(25.0, minor m 1 0)
    Assert.Equal(-25.0, cofactor m 1 0)

[<Fact>]
let ``calculating the determinant of 3x3 matrix`` () =
    let m = Matrix [ [1; 2; 6]
                     [-5; 8; -4]
                     [2; 6; 4] ]
    Assert.Equal(56.0, cofactor m 0 0)
    Assert.Equal(12.0, cofactor m 0 1)
    Assert.Equal(-46.0, cofactor m 0 2)
    Assert.Equal(-196.0, determinant m)

[<Fact>]
let ``calculating the determinant of 4x4 matrix`` () =
    let m = Matrix [ [-2; -8; 3; 5]
                     [-3; 1; 7; 3]
                     [1; 2; -9; 6]
                     [-6; 7; 7; -9] ]
    Assert.Equal(690.0, cofactor m 0 0)
    Assert.Equal(447.0, cofactor m 0 1)
    Assert.Equal(210.0, cofactor m 0 2)
    Assert.Equal(51.0, cofactor m 0 3)
    Assert.Equal(-4071.0, determinant m)

[<Fact>]
let ``testing an invertable matrix for invertability`` () =
    let m = Matrix [ [6; 4; 4; 4]
                     [5; 5; 7; 6]
                     [4; -9; 3; -7]
                     [9; 1; 7; -6] ]
    Assert.Equal(-2120.0, determinant m)
    Assert.True(invertible m)

[<Fact>]
let ``testing a noninvertable matrix for invertability`` () =
    let m = Matrix [ [-4; 2; -2; -3]
                     [9; 6; 2; 6]
                     [0; -5; 1; -5]
                     [0; 0; 0; 0] ]
    Assert.Equal(0.0, determinant m)
    Assert.False(invertible m)

[<Fact>]
let ``calculating the inverse of a matrix`` () =
    let m = Matrix [ [-5; 2; 6; -8]
                     [1; -5; 1; 8]
                     [7; 7; -6; -7]
                     [1; -3; 7; 4] ]
    let expected = Matrix [ [ 0.21805;  0.45113;  0.24060; -0.04511]
                            [-0.80827; -1.45677; -0.44361;  0.52068]
                            [-0.07895; -0.22368; -0.05263;  0.19737]
                            [-0.52256; -0.81391; -0.30075;  0.30639] ]
    let n = inverse m
    Assert.Equal(532.0, determinant m)
    Assert.Equal(-160.0, cofactor m 2 3)
    Assert.Equal(-160.0/532.0, n.[3, 2])
    Assert.Equal(105.0, cofactor m 3 2)
    Assert.Equal(105.0/532.0, n.[2, 3])
    Assert.Equal(expected, round n)

[<Fact>]
let ``calculating the inverse of another matrix`` () =
    let m = Matrix [ [8; -5; 9; 2]
                     [7; 5; 6; 1]
                     [-6; 0; 9; 6]
                     [-3; 0; -9; -4] ]
    let expected = Matrix [ [-0.15385; -0.15385; -0.28205; -0.53846]
                            [-0.07692;  0.12308;  0.02564;  0.03077]
                            [ 0.35897;  0.35897;  0.43590;  0.92308]
                            [-0.69231; -0.69231; -0.76923; -1.92308] ]
    Assert.Equal (expected, m |> inverse |> round)

[<Fact>]
let ``calculating the inverse of a third matrix`` () =
    let m = Matrix [ [ 9;  3;  0;  9]
                     [-5; -2; -6; -3]
                     [-4;  9;  6;  4]
                     [-7;  6;  6;  2] ]
    let expected = Matrix [ [-0.04074; -0.07778;  0.14444; -0.22222]
                            [-0.07778;  0.03333;  0.36667; -0.33333]
                            [-0.02901; -0.14630; -0.10926;  0.12963]
                            [ 0.17778;  0.06667; -0.26667;  0.33333] ]
    Assert.Equal (expected, m |> inverse |> round)

[<Fact>]
let ``multiplying a product by its inverse`` () =
    let m = Matrix [ [ 3; -9;  7;  3]
                     [ 3; -8;  2; -9]
                     [-4;  4;  4;  1]
                     [-6;  5; -1;  1] ]
    let n = Matrix [ [ 8;  2;  2;  2]
                     [ 3; -1;  7;  0]
                     [ 7;  0;  5;  4]
                     [ 6; -2;  0;  5] ]
    let p = m * n
    Assert.Equal(m, (p * inverse n) |> round)
