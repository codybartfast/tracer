module Camera

open System

open Canvas
open Matrix
open Primitives
open Ray
// open Transform
open World

type Camera(hSize: int, vSize: int, fieldOfView: float, transform: Matrix) =
    let halfView = fieldOfView / 2.0 |> Math.Tan
    let aspect = (float hSize) / (float vSize)
    let (halfWidth, halfHeight) =
        if aspect >= 1.0 then
            (halfView, halfView / aspect)
        else
            (halfView * aspect, halfView)
    let pixelSize = (halfWidth * 2.0) / (float hSize)

    new(hSize, vSize, fieldOfView) =
        Camera(hSize, vSize, fieldOfView, identity ())
    member _.HSize = hSize
    member _.HalfWidth = halfWidth
    member _.VSize = vSize
    member _.HalfHeight = halfHeight
    member _.FieldOfView = fieldOfView
    member _.Transform = transform
    member _.PixelSize = pixelSize
    member c.With(?hSize: int,
                  ?vSize: int,
                  ?fieldOfView: float,
                  ?transform: Matrix) =
        let hSize = defaultArg hSize c.HSize
        let vSize = defaultArg vSize c.VSize
        let fieldOfView = defaultArg fieldOfView c.FieldOfView
        let transform = defaultArg transform c.Transform
        Camera(hSize, vSize, fieldOfView, transform)


let camera hSize vSize fieldOfView transform =
    Camera(hSize, vSize, fieldOfView, transform)

let rayForPixel (camera: Camera) (px: int) (py: int) =
    let (px, py) = (float px, float py)
    let xOffset = (px + 0.5) * camera.PixelSize
    let yOffset = (py + 0.5) * camera.PixelSize
    let worldX = camera.HalfWidth - xOffset
    let worldY = camera.HalfHeight - yOffset
    let pixel = inverse camera.Transform *. point worldX worldY -1.0
    let origin = inverse camera.Transform *. zeroPoint
    let direction = pixel - origin |> normalize
    ray origin direction

let render (camera: Camera) (world: World) (reflectDepth: int)=
    let image = canvas camera.HSize camera.VSize
    for y in 0 .. camera.VSize - 1 do
        for x in 0 .. camera.HSize - 1 do
            let ray = rayForPixel camera x y
            let color = world.ColorAt(ray, reflectDepth)
            writePixel image x y color |> ignore
    image

