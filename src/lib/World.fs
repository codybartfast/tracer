module World

open Primitives
open Ray
open Sphere
open Transformations

type World(lights: PointLight list, spheres: Sphere list) =
    new() = World([], [])
    member _.Item with get(i) = spheres.[i]
    member _.IsEmpty = List.isEmpty spheres
    member _.Lights = lights
    member _.FirstLight = match lights with l::_ -> Some l | _ -> None
    member _.Spheres = spheres
    member _.Contains(predicate: Sphere -> bool) : bool =
        List.exists predicate spheres
    member _.Intersect r =
        spheres
            |> List.collect (intersect r)
            |> List.filter (fun i -> i.T >= 0.0)
            |> List.sortBy (fun i -> i.T)
    member w.With(?lights: PointLight list, ?spheres: Sphere list) =
        let lights = defaultArg lights w.Lights
        let spheres = defaultArg spheres w.Spheres
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
    World([light], [s1; s2])

let intersectWorld (w: World) r = w.Intersect(r)

let isShadowed (world: World) (light: Option<PointLight>) (point: Point) =
    match light with
    | None -> true
    | Some light ->
        let v = light.Position - point
        let distance = magnitude v
        let direction = normalize v
        let r = ray point direction
        let h = intersectWorld world r |> hit
        match h with
        | Some h when h.T < distance -> true
        | _ -> false

let shadeHit (world: World) comps =
    world.Lights
        |> List.map (fun light ->
            lighting
                comps.Object.Material
                light
                comps.OverPoint
                comps.Eyev
                comps.Normalv
                (isShadowed world (Some light) comps.OverPoint))
        |> List.reduce (+)

let colorAt w r =
    match intersectWorld w r |> hit with
    | None -> black
    | Some hit -> prepareComputations hit r |> shadeHit w
