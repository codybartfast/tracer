module World

open Primitives
open Ray
open ShapeBase
open Shapes
open Transformations

type World(lights: PointLight list, shapes: Shape list) =
    new() = World([], [])
    member _.Item with get(i) = shapes.[i]
    member _.IsEmpty = List.isEmpty shapes
    member _.Lights = lights
    member _.FirstLight = match lights with l::_ -> Some l | _ -> None
    member _.Shapes = shapes
    member _.Contains(predicate: Shape -> bool) : bool =
        List.exists predicate shapes
    member _.Intersect r =
        shapes
            |> List.collect (intersect r)
            |> List.filter (fun i -> i.T >= 0.0)
            |> List.sortBy (fun i -> i.T)
    member w.With(?lights: PointLight list, ?shapes: Shape list) =
        let lights = defaultArg lights w.Lights
        let shapes = defaultArg shapes w.Shapes
        World(lights, shapes)

let private defaultWorldMaterial =
    defaultMaterial
        .With(color = color 0.8 1.0 0.6,
              diffuse = 0.7,
              specular = 0.2)

let world light shapes = World([light], shapes)

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
                comps.Object
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
