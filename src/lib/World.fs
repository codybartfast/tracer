module World

open System

open Primitives
open Ray
open Transform

type World(light: Option<PointLight>, [<ParamArray>] spheres: Sphere[]) =
    let spheres = List.ofArray spheres
    member _.IsEmpty = List.isEmpty spheres
    member _.Light = light
    member _.Contains(predicate: Sphere -> bool) : bool =
        List.exists predicate spheres
    member _.Intersect r =
        spheres
            |> List.collect (intersect r)
            |> List.filter (fun i -> i.T >= 0.0)
            |> List.sortBy (fun i -> i.T)

let private defaultWorldMaterial =
    defaultMaterial
        .With(color = color 0.8 1.0 0.6,
              diffuse = 0.7,
              specular = 0.2)

let world () = World(None)

let defaultWorld () =
    let light = pointLight (pointi -10 10 -10) (colori 1 1 1)
    let s1 = Sphere(material = defaultWorldMaterial)
    let s2 = Sphere(scaling 0.5 0.5 0.5)
    World(Some light, s1, s2)

let intersectWorld (w: World) r = w.Intersect(r)