module Chapter8

open System.IO

open Camera
open Canvas
open Primitives
open ShapeBase
open Sphere
open Transformations
open World


let chapter8 width height =
    let material =
        defaultMaterial.With(color = white, specular = 0.0)

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
            .With(color = lightRed, diffuse = 0.7, specular = 0.3)

    let middle = Sphere(translation -0.5 1.0 0.5, material)

    let right =
        Sphere(
            translation 1.5 0.5 -0.5 * scaling 0.5 0.5 0.5,
            material.With(color = lighten lightBlue))

    let left =
        Sphere(
            translation -1.5 0.33 -0.75 * scaling 0.33 0.33 0.33,
            material.With(color = lightGreen))


    let light1 = pointLight (pointi -10 10 -10) (color 0.7 0.55 0.4)
    let light2 = pointLight (pointi -2 4 0) (color 0.4 0.55 0.7)
    let spheres = [floor; leftWall; rightWall; middle; left; right]
    let world = World([light1; light2], spheres)

    let camera =
        viewTransform (point 3.0 1.2 -3.6) (pointi 0.7 1.0 0.0) (vectori 0 1 0)
        |> camera width height (pi / 3.0)

    let canv = render camera world
    let filename = "Ch8Scene.ppm"
    File.WriteAllText(filename, canvasToPpm canv)
    printfn $"Written ppm to {FileInfo(filename).FullName}"
    ()
