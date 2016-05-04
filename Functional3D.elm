module Functional3D (..) where

import Color exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Array


type alias Drawing3DContext =
  { color : Color
  , angle : Float
  }


type Drawing3D
  = DF (Drawing3DContext -> List Renderable)


($) : Drawing3D -> Drawing3D -> Drawing3D
($) (DF a) (DF b) =
  DF
    (\ctx ->
      a ctx ++ b ctx
    )



-- rotate : ( Float, Float, Float ) -> Drawing3D -> Drawing3D
-- rotate ( x, y, z ) (DF f) =
--   DF
--     (\ctx ->
--       -- TODO: rotate
--       -- x -> 1 0 0
--       -- y -> 0 1 0
--       -- z -> 0 0 1
--       f ctx
--      -- TODO: rotate back
--      -- -x -> 1 0 0
--      -- -y -> 0 1 0
--      -- -z -> 0 0 1
--     )


angle : Float -> Drawing3D -> Drawing3D
angle a (DF f) =
  DF
    (\ctx ->
      f { ctx | angle = ctx.angle + a }
    )


translate : ( Float, Float, Float ) -> Drawing3D -> Drawing3D
translate ( x, y, z ) (DF f) =
  DF
    (\ctx ->
      -- TODO: translate (x, y, z)
      f ctx
     -- TODO: translate back (-x, -y, -z)
    )


color : Color -> Drawing3D -> Drawing3D
color clr (DF f) =
  DF
    (\ctx ->
      f { ctx | color = clr }
    )


empty : Drawing3D
empty =
  DF (\ctx -> [])


getColor ctx =
  let
    c =
      toRgb ctx.color
  in
    vec3
      (toFloat c.red / 255)
      (toFloat c.green / 255)
      (toFloat c.blue / 255)


cube : Drawing3D
cube =
  DF
    (\ctx ->
      let
        color =
          getColor ctx

        rft =
          vec3 1 1 1

        lft =
          vec3 -1 1 1

        lbt =
          vec3 -1 -1 1

        rbt =
          vec3 1 -1 1

        rbb =
          vec3 1 -1 -1

        rfb =
          vec3 1 1 -1

        lfb =
          vec3 -1 1 -1

        lbb =
          vec3 -1 -1 -1

        cube =
          Triangle
            << List.concat
            <| [ face color rft rfb rbb rbt
               , face color rft rfb lfb lft
               , face color rft lft lbt rbt
               , face color rfb lfb lbb rbb
               , face color lft lfb lbb lbt
               , face color rbt rbb lbb lbt
               ]
      in
        [ render vertexShader fragmentShader cube (uniforms ctx.angle) ]
    )


unsafeGet : Int -> Array.Array a -> a
unsafeGet idx array =
  case Array.get idx array of
    Just x ->
      x

    Nothing ->
      Debug.crash ("Could not get: " ++ toString idx)


cylinder : Drawing3D
cylinder =
  DF
    (\ctx ->
      let
        color =
          getColor ctx

        quality =
          40

        q =
          pi / toFloat quality / 2.0

        circlePoints =
          [0..quality]
            |> List.map
                (\i ->
                  ( sin (toFloat i * q) * 0.5, cos (toFloat i * q) * 0.5 )
                )
            |> Array.fromList

        borderCircles =
          [ -0.5, 0.5 ]
            |> List.map
                (\hy ->
                  circlePoints
                    |> Array.map (\( x, y ) -> vec3 x y hy)
                )
            |> Array.fromList

        vertex position =
          Vertex color position
      in
        [0..quality - 1]
          |> List.concatMap
              (\i ->
                let
                  bc0 =
                    borderCircles
                      |> unsafeGet 0
                      |> unsafeGet i

                  bc1 =
                    borderCircles
                      |> unsafeGet 0
                      |> unsafeGet (i + 1)

                  triangles : List TriangleType
                  triangles =
                    [ -- First triangle of the rounded part
                      N (vec3 (getX bc0) (getY bc0) 0.0)
                    , V (vertex bc0)
                    , N (vec3 (getX bc1) (getY bc1) 0.0)
                    , V (vertex bc1)
                    , V (vertex bc1)
                      -- Second triangle of the rounded part
                    ]
                in
                  []
              )
    )


type alias Vertex =
  { color : Vec3
  , position : Vec3
  }


type TriangleType
  = N Vec3
  | V Vertex


face color a b c d =
  let
    vertex position =
      Vertex color position
  in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]


uniforms t =
  { rotation =
      makeRotate 90 (vec3 0 1 0)
      --mul (makeRotate (3 * t) (vec3 0 1 0)) (makeRotate (2 * t) (vec3 1 0 0))
  , perspective = makePerspective 45 1 1.0e-2 0
  , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
  , shade = 0.8
  }


vertexShader =
  [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * rotation * vec4(position, 1.0);
    vcolor = color;
}

|]


fragmentShader =
  [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]

