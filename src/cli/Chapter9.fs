module Chapter9

open System.IO

open Camera
open Canvas
open Primitives
open ShapeBase
open Shapes
open Transformations
open World


let chapter9 width height =
    let floor = plane ()

    let walls =
        [0..5] |> List.map (fun n ->
            let roty = rotationY ((float n) * (pi / 3.0) + 0.5)
            Plane(  roty
                    * translationi 0 0 7
                    * rotationX (pi / 2.0),
                floor.Material) :> Shape
        )

    let material =
        defaultMaterial
            .With(color = lightRed, diffuse = 0.7, specular = 0.3)

    let middle = Sphere(translation -0.5 0.5 0.5, material)

    let right =
        Sphere(
            translation 1.5 0.5 -0.5 * scaling 0.5 0.5 0.5,
            material.With(color = lighten lightBlue))

    let left =
        Sphere(
            translation -1.5 0.33 -0.75 * scaling 0.33 0.33 0.33,
            material.With(color = lightGreen))


    let light1 = pointLight (pointi -5 5 -5) (color 0.7 0.55 0.4)
    let light2 = pointLight (pointi -2 4 0) (color 0.4 0.55 0.7)
    let shapes : Shape list = [floor; middle; left; right] @ walls
    let world = World([light1; light2], shapes)

    let camera =
        viewTransform (point 3.0 1.2 -3.6) (pointi 0.7 1.0 0.0) (vectori 0 1 0)
        |> camera width height (pi / 3.0)

    let canv = render camera world 0
    let filename = "Ch9Plane.ppm"
    File.WriteAllText(filename, canvasToPpm canv)
    printfn $"Written ppm to {FileInfo(filename).FullName}"
    ()
