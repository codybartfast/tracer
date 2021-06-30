module World

open System

open Primitives
open Ray
open Transform

type World(lights: PointLight list, [<ParamArray>] spheres: Sphere[]) =
    let spheresList = List.ofArray spheres
    new() = World([])
    member _.Item with get(i) = spheres.[i]
    member _.IsEmpty = List.isEmpty spheresList
    member _.Lights = lights
    member _.Contains(predicate: Sphere -> bool) : bool =
        List.exists predicate spheresList
    member _.Intersect r =
        spheresList
            |> List.collect (intersect r)
            |> List.filter (fun i -> i.T >= 0.0)
            |> List.sortBy (fun i -> i.T)
    member w.With(?lights: PointLight list, [<ParamArray>] ?spheres: Sphere[]) =
        // Surprised [<ParamArray>] can come after an optional param.
        let lights = defaultArg lights w.Lights
        let spheres = defaultArg spheres (spheresList |> Array.ofList)
        World(lights, spheres)

let private defaultWorldMaterial =
    defaultMaterial
        .With(color = color 0.8 1.0 0.6,
              diffuse = 0.7,
              specular = 0.2)

let world light spheres = World([light], spheres)

let defaultWorld () =
    let light = pointLight (pointi -10 10 -10) (colori 1 1 1)
    let s1 = Sphere(material = defaultWorldMaterial)
    let s2 = Sphere(scaling 0.5 0.5 0.5)
    World([light], s1, s2)

let intersectWorld (w: World) r = w.Intersect(r)

let shadeHit (world: World) comps =
    (black, world.Lights) ||> List.fold (fun c light ->
        c +  lighting
                comps.Object.Material
                light
                comps.Point
                comps.Eyev
                comps.Normalv)
