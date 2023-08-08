module Background exposing (drawBackground)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Messages exposing (Msg)
import String


drawBackground : Float -> Float -> Float -> Float -> List (Svg Msg)
drawBackground width_ height_ lineX topBallRadius =
    let middle = String.fromFloat lineX
        topRadiusStr = String.fromFloat topBallRadius
        heightStr = String.fromFloat height_
    in
    [ rect [ width (String.fromFloat width_)
           , height heightStr
           , fill "#dbfacf"
           , x "0"
           , y "0"
           ] []
    , line [ x1 middle
           , x2 middle
           , y1 "0"
           , y2 heightStr
           , stroke "#728687", strokeWidth "1"
           ] []
    , circle [ cx middle
             , cy topRadiusStr
             , r topRadiusStr
             , fill "black"
             ] []
    ]
