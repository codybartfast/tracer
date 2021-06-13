module Transform

open Matrix

let translation x y z =
    let t = identity ()
    t.[0, 3] <- x
    t.[1, 3] <- y
    t.[2, 3] <- z
    t
