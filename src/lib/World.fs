module World

open System

open Primitives
open Ray
open Transform

type World(light: PointLight, [<ParamArray>] spheres: Sphere[]) =
    member _.IsEmpty = true
    member _.Light = None
    member _.Contains(predicate: Sphere -> bool) : bool =
        Array.exists predicate spheres
    // member _.Contains(s: Sphere) : bool =
    //     Array.contains s spheres

let world () =
    let light = pointLight (pointi -10 10 -10) (colori 1 1 1)
    let s1 = sphere()
                .With(material = material()
                    .With(color = color 0.8 1.0 0.6,
                                  diffuse = 0.7,
                                  specular = 0.2 ))
    let s2 = Sphere(scaling 0.5 0.5 0.5)
    World(light, s1, s2)
