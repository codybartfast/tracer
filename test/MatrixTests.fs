module MatrixTests

open System
open Xunit

open Matrix
open Tuple

type Assert = XUnitExtensions.TracerAssert

let matrixOfInts = matrixOfLists >> (Array2D.map float)
let round = Array2D.map (fun (n: float) -> Math.Round(n, 5))

[<Theory>]
[<InlineData(1.0, 0, 0)>]
[<InlineData(4.0, 0, 3)>]
[<InlineData(5.5, 1, 0)>]
[<InlineData(7.5, 1, 2)>]
[<InlineData(11.0, 2, 2)>]
[<InlineData(13.5, 3, 0)>]
[<InlineData(15.5, 3, 2)>]
let constructing_and_inspecting_a_4x4_matrix (expected, x, y) =
    let m =
        matrixOfLists [ [ 1.0; 2.0; 3.0; 4.0 ]
                        [ 5.5; 6.5; 7.5; 8.5 ]
                        [ 9.0; 10.0; 11.0; 12.0 ]
                        [ 13.5; 14.5; 15.5; 16.5 ] ]
    Assert.Equal(expected, m.[x, y])

[<Theory>]
[<InlineData(-3, 0, 0)>]
[<InlineData(5, 0, 1)>]
[<InlineData(1, 1, 0)>]
[<InlineData(-2, 1, 1)>]
let a_2x2_matrix_ought_to_be_representable (expected, x, y) =
    let m = matrixOfLists [ [ -3; 5 ]
                            [ 1; -2 ] ]
    Assert.Equal(expected, m.[x, y])

[<Theory>]
[<InlineData(-3, 0, 0)>]
[<InlineData(-2, 1, 1)>]
[<InlineData(1, 2, 2)>]
let a_3x3_matrix_ought_to_be_representable (expected, x, y) =
    let m = matrixOfLists [ [ -3; 5; 0 ]
                            [ 1; -2; -7 ]
                            [ 0; 1; 1] ]
    Assert.Equal(expected, m.[x, y])

[<Fact>]
let matrix_equality_with_identical_matrices () =
    let a = matrixOfLists [ [1; 2; 3; 4]
                            [5; 6; 7; 8]
                            [9; 8; 7; 6]
                            [5; 4; 3; 2] ]

    let b = matrixOfLists [ [1; 2; 3; 4]
                            [5; 6; 7; 8]
                            [9; 8; 7; 6]
                            [5; 4; 3; 2] ]
    Assert.True( (a = b) )

[<Fact>]
let matrix_equality_with_different_matrices () =
    let a = matrixOfLists [ [1; 2; 3; 4]
                            [5; 6; 7; 8]
                            [9; 8; 7; 6]
                            [5; 4; 3; 2] ]

    let b = matrixOfLists [ [2; 3; 4; 5]
                            [6; 7; 8; 9]
                            [8; 7; 6; 5]
                            [4; 3; 2; 1] ]

    Assert.True( (a <> b) )

[<Fact>]
let multiplying_two_matrices () =
    let a = matrixOfLists [ [1.0; 2.0; 3.0; 4.0]
                            [5.0; 6.0; 7.0; 8.0]
                            [9.0; 8.0; 7.0; 6.0]
                            [5.0; 4.0; 3.0; 2.0] ]
    let b = matrixOfLists [ [-2.0; 1.0; 2.0; 3.0]
                            [3.0; 2.0; 1.0; -1.0]
                            [4.0; 3.0; 6.0; 5.0]
                            [1.0; 2.0; 7.0; 8.0] ]
    let actual = a |*| b
    let expected = matrixOfLists [ [20.0; 22.0; 50.0; 48.0]
                                   [44.0; 54.0; 114.0; 108.0]
                                   [40.0; 58.0; 110.0; 102.0]
                                   [16.0; 26.0; 46.0; 42.0] ]
    Assert.True((actual = expected))


[<Fact>]
let a_matrix_multiplied_by_a_tuple () =
    // let A = matrixOfLists [ [0.0; 1.0; 2.0; 4.0]
    //                         [1.0; 2.0; 4.0; 8.0]
    //                         [2.0; 4.0; 8.0; 16.0]
    //                         [4.0; 8.0; 16.0; 32.0] ]
    let A = matrixOfLists [ [0.0; 1.0; 2.0]
                            [1.0; 2.0; 4.0]
                            [2.0; 4.0; 8.0] ]
    let b = vector 1.0 2.0 3.0
    // let expected = vector 18.0 24.0 33.0
    let expected = vector 8.0 17.0 34.0
    Assert.TupleEqual (expected, A |* b)

[<Fact>]
let multiplying_a_matrix_by_identity_matrix () =
    // let A = matrixOfLists [ [0.0; 1.0; 2.0; 4.0]
    //                         [1.0; 2.0; 4.0; 8.0]
    //                         [2.0; 4.0; 8.0; 16.0]
    //                         [4.0; 8.0; 16.0; 32.0] ]
    let A = matrixOfLists [ [0.0; 1.0; 2.0]
                            [1.0; 2.0; 4.0]
                            [2.0; 4.0; 8.0] ]
    let I = identityMatrix
    Assert.Equal(A, A |*| I)

[<Fact>]
let multiplying_the_identity_matrix_by () =
    let a = vector 1.0 2.0 3.0
    Assert.Equal(a, identityMatrix |* a)

[<Fact>]
let transposing_a_matrix () =
    let A  = matrixOfLists [ [0; 9; 3; 0]
                             [9; 8; 0; 8]
                             [1; 8; 5; 3]
                             [0; 0; 5; 8] ]
    let expected  = matrixOfLists [ [0; 9; 1; 0]
                                    [9; 8; 8; 0]
                                    [3; 0; 5; 5]
                                    [0; 8; 3; 8] ]
    Assert.Equal(expected, transpose A)
    Assert.Equal(A, A |> transpose |> transpose)

[<Fact>]
let transpose_the_identity_matrix () =
    Assert.Equal(identityMatrix, transpose identityMatrix)

[<Fact>]
let calculating_the_determinant_of_a_2x2_matrix () =
    let A = matrixOfInts [ [1; 5]
                           [-3; 2]]
    Assert.Equal(17.0, determinant A)

[<Fact>]
let a_submatrix_of_a_3x3_matrix_is_a_2x2_matrix () =
    let A = matrixOfInts [ [1; 5; 0]
                           [-3; 2; 7]
                           [0; 6; -3] ]
    let expected = matrixOfInts [ [-3; 2]
                                  [0; 6] ]
    Assert.Equal(expected, submatrix A 0 2)

[<Fact>]
let a_submatrix_of_a_4x4_matrix_is_a_3x3_matrix () =
    let A = matrixOfInts [ [-6; 1; 1; 6]
                           [-8; 5; 8; 6]
                           [-1; 0; 8; 1]
                           [-7; 1; -1; 1] ]
    let expected = matrixOfInts [ [-6; 1; 6]
                                  [-8; 8; 6]
                                  [-7; -1; 1] ]
    Assert.Equal(expected, submatrix A 2 1)

[<Fact>]
let calculating_a_minor_of_a_3x3_matrix () =
    let A = matrixOfInts [ [3; 5; 0]
                           [2; -1; -7]
                           [6; -1; 5] ]
    let B = submatrix A 1 0
    Assert.Equal(25.0, determinant B)
    Assert.Equal(25.0, minor A 1 0)

[<Fact>]
let calculating_a_cofactor_of_a_3x3_matrix () =
    let A = matrixOfInts [ [3; 5; 0]
                           [2; -1; -7]
                           [6; -1; 5] ]
    Assert.Equal(-12.0, minor A 0 0)
    Assert.Equal(-12.0, cofactor A 0 0)
    Assert.Equal(25.0, minor A 1 0)
    Assert.Equal(-25.0, cofactor A 1 0)

[<Fact>]
let calculating_the_determinant_of_3x3_matrix () =
    let A = matrixOfInts [ [1; 2; 6]
                           [-5; 8; -4]
                           [2; 6; 4] ]
    Assert.Equal(56.0, cofactor A 0 0)
    Assert.Equal(12.0, cofactor A 0 1)
    Assert.Equal(-46.0, cofactor A 0 2)
    Assert.Equal(-196.0, determinant A)

[<Fact>]
let calculating_the_determinant_of_4x4_matrix () =
    let A = matrixOfInts [ [-2; -8; 3; 5]
                           [-3; 1; 7; 3]
                           [1; 2; -9; 6]
                           [-6; 7; 7; -9] ]
    Assert.Equal(690.0, cofactor A 0 0)
    Assert.Equal(447.0, cofactor A 0 1)
    Assert.Equal(210.0, cofactor A 0 2)
    Assert.Equal(51.0, cofactor A 0 3)
    Assert.Equal(-4071.0, determinant A)

[<Fact>]
let testing_an_invertable_matrix_for_invertability () =
    let A = matrixOfInts [ [6; 4; 4; 4]
                           [5; 5; 7; 6]
                           [4; -9; 3; -7]
                           [9; 1; 7; -6] ]
    Assert.Equal(-2120.0, determinant A)
    Assert.True(invertible A)

[<Fact>]
let testing_a_noninvertable_matrix_for_invertability () =
    let A = matrixOfInts [ [-4; 2; -2; -3]
                           [9; 6; 2; 6]
                           [0; -5; 1; -5]
                           [0; 0; 0; 0] ]
    Assert.Equal(0.0, determinant A)
    Assert.False(invertible A)

[<Fact>]
let calculating_the_inverse_of_a_matrix () =
    let A = matrixOfInts [ [-5; 2; 6; -8]
                           [1; -5; 1; 8]
                           [7; 7; -6; -7]
                           [1; -3; 7; 4] ]
    let expectedB = matrixOfLists [ [ 0.21805;  0.45113;  0.24060; -0.04511]
                                    [-0.80827; -1.45677; -0.44361;  0.52068]
                                    [-0.07895; -0.22368; -0.05263;  0.19737]
                                    [-0.52256; -0.81391; -0.30075;  0.30639] ]
    let B = inverse A
    Assert.Equal(532.0, determinant A)
    Assert.Equal(-160.0, cofactor A 2 3)
    Assert.Equal(-160.0/532.0, B.[3, 2])
    Assert.Equal(105.0, cofactor A 3 2)
    Assert.Equal(105.0/532.0, B.[2, 3])
    let B = B |> Array2D.map (fun n -> Math.Round(n, 5))
    Assert.Equal(expectedB, B)

[<Fact>]
let calculating_the_inverse_of_another_matrix () =
    let A = matrixOfInts [ [8; -5; 9; 2]
                           [7; 5; 6; 1]
                           [-6; 0; 9; 6]
                           [-3; 0; -9; -4] ]
    let expected = matrixOfLists [ [-0.15385; -0.15385; -0.28205; -0.53846]
                                   [-0.07692;  0.12308;  0.02564;  0.03077]
                                   [ 0.35897;  0.35897;  0.43590;  0.92308]
                                   [-0.69231; -0.69231; -0.76923; -1.92308] ]
    Assert.Equal (expected, A |> inverse |> round)

[<Fact>]
let calculating_the_inverse_of_a_third_matrix () =
    let A = matrixOfInts [ [ 9;  3;  0;  9]
                           [-5; -2; -6; -3]
                           [-4;  9;  6;  4]
                           [-7;  6;  6;  2] ]
    let expected = matrixOfLists [ [-0.04074; -0.07778;  0.14444; -0.22222]
                                   [-0.07778;  0.03333;  0.36667; -0.33333]
                                   [-0.02901; -0.14630; -0.10926;  0.12963]
                                   [ 0.17778;  0.06667; -0.26667;  0.33333] ]
    Assert.Equal (expected, A |> inverse |> round)

[<Fact>]
let multiplying_a_product_by_its_inverse () =
    let A = matrixOfInts [ [ 3; -9;  7;  3]
                           [ 3; -8;  2; -9]
                           [-4;  4;  4;  1]
                           [-6;  5; -1;  1] ]
    let B = matrixOfInts [ [ 8;  2;  2;  2]
                           [ 3; -1;  7;  0]
                           [ 7;  0;  5;  4]
                           [ 6; -2;  0;  5] ]
    let C = A |*| B
    Assert.Equal(A, (C |*| inverse B) |> round)
