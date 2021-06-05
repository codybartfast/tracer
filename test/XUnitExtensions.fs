module XUnitExtensions

open Xunit

type TracerAssert () = 
    inherit Assert ()

    static member TupleEqual (a, b) = 
        if not (Tuple.equal a b) then
            raise (Sdk.EqualException(a, b))
    
    static member ValEqual (a, b) =
        if not (Tuple.valEqual a b) then
            raise (Sdk.EqualException(a, b))


// let assertTupleEqual a b = Assert.True(equal a b)
// 