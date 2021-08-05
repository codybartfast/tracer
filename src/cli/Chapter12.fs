module Chapter12

open System.IO

open Camera
open Canvas
open Patterns
open Primitives
open ShapeBase
open Shapes
open Transformations
open World

let chapter12 picPath width height  =
    let floorPat =
        blendPatterns [
            solidWhite
            solidWhite
            squareLimit 0 (BitmapPattern("bmp/Puffin.jpg"))]
    let floor =
        Plane(
            translationi 23 0 -8 * scalingi 12 12 12,
            material.With(pattern = floorPat))

    let ceilingPat =
        StripePattern(
            blendColors [lightCyan; lightBlue; darkGrey] |> solid,
            solid lightGrey)
    let ceiling =
        Plane(
            translation 0.0 5.2 0.0 * rotationY (pi / 6.0),
            material
                .With(
                    pattern = ceilingPat,
                    ambient = 0.5))

    let wallPat = scaling 0.3 0.3 0.3 * BitmapPattern("bmp/Bricks.jpg")
    let georgeWallPat =
        let george =
            translation 0.62 0.0 0.4 *
            scaling 0.6 0.6 0.6 *
            (BitmapPattern("bmp/George.png") |> flippedPairs)
        let george = blendPatterns [george; solidBlack]
        addPatterns [george; wallPat]
    let wallMat = material.With(pattern = wallPat, shininess = 999999.9)

    let walls =
        let walls =
            [0 .. 5] |> List.map (fun n ->
            // [0; 4; 5] |> List.map (fun n ->
                let roty = rotationY ((float n) * (pi / 3.0) + 0.5)
                Plane(  roty
                        * translation 0.0 0.46 7.0
                        * rotationX (pi / 2.0)
                        * scalingi 5 5 5,
                    wallMat ) )
        match walls with
        | [] -> []
        | rightWall::rest ->
            let rightWall =
                rightWall.With(material = wallMat.With(pattern = georgeWallPat))
            (rightWall::rest) |> List.map(fun wall -> wall :> Shape)


    let left =
        Sphere(
            translation -0.6 0.5 -1.0
                * scaling 0.5 0.5 0.5
                * rotationY 0.5
                * rotationX -halfPi,
            material
                .With(
                    color = darkGrey,
                    reflective = 0.15,
                    ambient = 0.2))

    let material = material.With(diffuse = 0.7, specular = 0.3)
    let middlePat =
        BitmapPattern(picPath) |> squareLimit 4 |> InvMercatorPattern
    let middle1 =
        Sphere(
            translation -0.5 1.0 0.5
                // * scaling 0.6 0.6 0.6
                * rotationZ -0.2
                * rotationX -0.8
                * rotationY -2.2
                * rotationX -halfPi,
            material
                .With(
                    // reflective = 0.1,
                    pattern = middlePat,
                    ambient = 0.3))

    let transMat =
        defaultMaterial
            .With(
                // ambient = 0.0,
                // diffuse = 0.0,
                color = black,
                shininess = 300.0,
                specular = 1.0,
                refractiveIndex = 2.0,
                reflective = 1.0,
                transparency = 0.4)

    let trans1 = Sphere(translation 1.9 1.2 2.2 * scaling 0.8 0.8 0.8, transMat)

    let trans2 = 
        Cube(
            translation 1.9 0.2 2.2 * scaling 0.8 0.2 0.8, 
            material.With(
                color = color 0.545 0.27 0.075,
                specular = 2.0,
                reflective = 0.02))

    let right =
        let rightPattern =
            BitmapPattern("bmp/Mercator.jpg")|>  InvMercatorPattern
        Sphere(
            translation 1.5 0.57 -0.5
                * scaling 0.57 0.57 0.57
                * rotationZ -0.3
                * rotationX -0.3
                * rotationY 0.6
                * rotationX -halfPi,
            material.With(pattern =  rightPattern, ambient = 0.08))


    let light1 = pointLight (pointi -5 5 -5) (color 0.7 0.55 0.4)
    let light2 = pointLight (pointi -2 4 0) (color 0.4 0.55 0.7)
    let shapes : Shape list =
        [left; middle1; right; trans1; trans2; floor; ceiling] @ walls
    let world = World([light1; light2], shapes)
    let camera =
        viewTransform (point 3.0 1.2 -3.6) (pointi 0.7 1.0 0.0) (vectori 0 1 0)
        |> camera width height (pi / 3.0)
    // let camera =
    //     viewTransform (point 12.2 2.0 -18.0) (pointi 0.7 1.0 0.0) (vectori 0 1 0)
    //     |> camera width height (pi / 14.0)

    let canv = render camera world 3
    let filename = "Ch12Cube.ppm"
    File.WriteAllText(filename, canvasToPpm canv)
    printfn $"Written ppm to {FileInfo(filename).FullName}"
    ()
