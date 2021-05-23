module Tests

open Xunit

[<Fact>]
let shouldBePolite () =
    Assert.Equal(Say.hello "Jamis", "Hello, Jamis!")
