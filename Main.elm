module Main (..) where

import Color exposing (..)
import Graphics.Element exposing (Element)
import WebGL exposing (..)
import Functional3D as F3D exposing (..)
import Time exposing (fps)


angleSignal : Signal Float
angleSignal =
  Signal.foldp (\dt theta -> 0) 0 (fps 25) -- theta + dt / 5000) 0 (fps 25)


ctx : Drawing3DContext
ctx =
  { color = green
  , angle = 640
  }


scene : Drawing3D -> Float -> List Renderable
scene (DF f) angle =
  f { ctx | angle = angle }


form =
  color blue cube $ color red (angle 90 cube)


main : Signal Element
main =
  Signal.map (webgl ( 400, 400 )) (Signal.map (scene form) angleSignal)

