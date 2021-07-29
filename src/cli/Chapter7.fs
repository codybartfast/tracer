module Chapter7

open System.IO

open Camera
open Canvas
open Primitives
open ShapeBase
open Shapes
open Transformations
open World


let chapter7 width height =
    let material =
        defaultMaterial.With(color = color 1.0 0.9 0.9, specular = 0.0)

    let floor = Sphere(scaling 10.0 0.01 10.0, material)

    let leftWall =
        Sphere(
            translationi 0 0 5
                * rotationY (-pi / 4.0)
                * rotationX (pi / 2.0)
                * scaling 10.0 0.01 10.0,
            floor.Material)

    let rightWall =
        Sphere(
            translationi 0 0 5
                * rotationY (pi / 4.0)
                * rotationX (pi / 2.0)
                * scaling 10.0 0.01 10.0,
            floor.Material)

    let material =
        defaultMaterial
            .With(color = color 0.1 1.0 0.5, diffuse = 0.7, specular = 0.3)

    let middle = Sphere(translation -0.5 1.0 0.5, material)

    let right =
        Sphere(
            translation 1.5 0.5 -0.5 * scaling 0.5 0.5 0.5,
            material.With(color = lightBlue))

    let left =
        Sphere(
            translation -1.5 0.33 -0.75 * scaling 0.33 0.33 0.33,
            material.With(color = color 1.0 0.8 0.1))


    let light = pointLight (pointi -10 10 -10) (colori 1 1 1)
    let shperes : Shapes = [floor; leftWall; rightWall; middle; left; right]
    let world = world light shperes

    let camera =
        viewTransform (point 0.0 1.5 -5.0) (pointi 0 1 0) (vectori 0 1 0)
        |> camera width height (pi / 3.0)

    let canv = render camera world 0
    let filename = "Ch7Scene.ppm"
    File.WriteAllText(filename, canvasToPpm canv)
    printfn $"Written ppm to {FileInfo(filename).FullName}"
    ()
