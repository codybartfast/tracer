module Transform

open System

open Matrix

let pi = Math.PI

let translation x y z =
    let t = identity ()
    t.[0, 3] <- x
    t.[1, 3] <- y
    t.[2, 3] <- z
    t

let scaling x y z =
    let t = zeroMatrix ()
    t.[0, 0] <- x
    t.[1, 1] <- y
    t.[2, 2] <- z
    t.[3, 3] <- 1.0
    t

let rotationX r =
    let t = zeroMatrix ()
    t.[0, 0] <- 1.0
    t.[1, 1] <- Math.Cos r
    t.[1, 2] <- - (Math.Sin r)
    t.[2, 1] <- Math.Sin r
    t.[2, 2] <- Math.Cos r
    t.[3, 3] <- 1.0
    t

let rotationY r =
    let t = zeroMatrix ()
    t.[0, 0] <- Math.Cos r
    t.[0, 2] <- Math.Sin r
    t.[1, 1] <- 1.0
    t.[2, 0] <- - (Math.Sin r)
    t.[2, 2] <- Math.Cos r
    t.[3, 3] <- 1.0
    t

let rotationZ r =
    let t = zeroMatrix ()
    t.[0, 0] <- Math.Cos r
    t.[0, 1] <- - (Math.Sin r)
    t.[1, 0] <- Math.Sin r
    t.[1, 1] <- Math.Cos r
    t.[2, 2] <- 1.0
    t.[3, 3] <- 1.0
    t

let shearing xy xz yx yz zx zy =
    let t = identity ()
    t.[0, 1] <- xy
    t.[0, 2] <- xz
    t.[1, 0] <- yx
    t.[1, 2] <- yz
    t.[2, 0] <- zx
    t.[2, 1] <- zy
    t
