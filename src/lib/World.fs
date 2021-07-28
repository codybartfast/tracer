module World

open Primitives
open Ray
open ShapeBase
open Shapes
open Transformations

let sr2 = sqrt 2.0
let hsr2 = sr2 / 2.0

type World(lights: PointLight list, shapes: Shape list) =
    new() = World([], [])
    member w.With(?lights: PointLight list, ?shapes: Shape list) =
        let lights = defaultArg lights w.Lights
        let shapes = defaultArg shapes w.Shapes
        World(lights, shapes)
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
    member w.IsShadowed(light: Option<PointLight>, point: Point) =
        match light with
        | None -> true
        | Some light ->
            let v = light.Position - point
            let distance = magnitude v
            let direction = normalize v
            let r = ray point direction
            let h = w.Intersect(r) |> hit
            match h with
            | Some h when h.T < distance -> true
            | _ -> false
    member w.ShadeHit(comps: Computations) =
        w.Lights
            |> List.map (fun light ->
                lighting
                    comps.Object.Material
                    comps.Object
                    light
                    comps.OverPoint
                    comps.Eyev
                    comps.Normalv
                    (w.IsShadowed(Some light, comps.OverPoint)))
            |> List.reduce (+)
    member w.ColorAt(ray: Ray) =
        match w.Intersect(ray) |> hit with
        | None -> black
        | Some hit -> prepareComputations hit ray |> w.ShadeHit
    member w.RefectedColor (comps: Computations) =
        let reflective = comps.Object.Material.Reflective
        match reflective with
        | 0.0 -> black
        | _ ->
            let reflectRay = ray comps.OverPoint comps.Reflectv
            let color = w.ColorAt(reflectRay)
            color * reflective


let private defaultWorldMaterial =
    defaultMaterial
        .With(color = color 0.8 1.0 0.6,
              diffuse = 0.7,
              specular = 0.2)

let world light shapes = World([light], shapes)

let defaultWorldLights = [ pointLight (pointi -10 10 -10) (colori 1 1 1)]
let defaultWorldS1 =  Sphere(material = defaultWorldMaterial)
let defaultWorldS2 =  Sphere(scaling 0.5 0.5 0.5)

let defaultWorld () =
    World(defaultWorldLights, [defaultWorldS1; defaultWorldS2])

let intersectWorld (w: World) r = w.Intersect(r)

let isShadowed (world: World) (light: Option<PointLight>) (point: Point) =
    world.IsShadowed(light, point)

let shadeHit (world: World) comps = world.ShadeHit(comps)

let colorAt (w: World) r = w.ColorAt(r)

