module Pendulum exposing (Pendulum, drawPendulum, updatePendula, buildPendula)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Messages exposing (..)
import List

{-| This has the state of a pendulum. All lengths are in centimeters and all angles in radians -}
type alias Pendulum =
    { weightRadius : Float
    , initialAngle : Float
    , currentAngle : Float
    , marked : Bool
    , stringLength : Float
    , tone : String
    }

buildPendula : Float -> Float -> List(Float) -> List(String) -> List(Pendulum)
buildPendula weightRadius initialAngle stringLengths tones =
    List.map2 (Pendulum weightRadius initialAngle initialAngle False) stringLengths tones

updatePendula : List(Pendulum) -> Float -> (List(Pendulum), List(String))
updatePendula pendula timeSeconds =
    let (newPendula, messages) = List.map (updatePendulum timeSeconds) pendula |> List.unzip
    in
        (newPendula, List.filterMap identity messages)

updatePendulum : Float -> Pendulum -> (Pendulum, Maybe String)
updatePendulum timeSeconds pendulum =
    let newAngle = getCurrentAngle pendulum timeSeconds
        newMarked = shouldHaveEffects pendulum newAngle timeSeconds
        command = if newMarked == pendulum.marked || not newMarked then Nothing else Just pendulum.tone
    in
        ({ pendulum | currentAngle = newAngle, marked = newMarked }, command)

radiansToDegreesStr : Float -> String
-- Adding `-` in front to keep the rotation direction positive (svg rotate produces clockwise rotations)
radiansToDegreesStr radians = -radians * 180 / pi |> String.fromFloat

drawPendulum : (Float, Float) -> Pendulum -> Svg.Svg Msg
drawPendulum ((originX, originY) as stringOrigin) pendulum =
    let d = distanceToWeightCenter pendulum
        (circleX, circleY) = getPosition stringOrigin d
        (stringX, stringY) = getPosition stringOrigin pendulum.stringLength
        rotationCenter = String.fromFloat originX ++ " " ++ String.fromFloat originY
        rotation = "rotate(" ++ radiansToDegreesStr pendulum.currentAngle ++ " " ++ rotationCenter ++ ")"
    in
        g [transform rotation]
            [ circle [ cx circleX
                     , cy circleY
                     , r (String.fromFloat pendulum.weightRadius)
                     , fill (circleFill pendulum.marked)
                     , stroke "black"
                     , strokeWidth "5"
                     ] []
            , line [ x1 stringX
                   , x2 (String.fromFloat originX)
                   , y1 stringY, y2 (String.fromFloat originY)
                   , stroke "black"
                   , strokeWidth "3"
                   ] []
            ]

-- Making this bigger makes the pendulums faster (this is the actual value in cm/(s^2))
gravitationalConstant : Float
gravitationalConstant = 981

distanceToWeightCenter : Pendulum -> Float
distanceToWeightCenter pendulum = pendulum.stringLength + pendulum.weightRadius

getCurrentAngle : Pendulum -> Float -> Float
getCurrentAngle  pendulum timeSeconds =
    let d = distanceToWeightCenter pendulum
    in
        pendulum.initialAngle * cos (sqrt (gravitationalConstant/d) * timeSeconds)

getPosition : (Float, Float) -> Float -> (String, String)
getPosition (originX, originY) distance =
    ( String.fromFloat originX
    , String.fromFloat (originY + distance)
    )


movementDirection : Pendulum -> Float -> Order
movementDirection pendulum timeSeconds =
    let d = distanceToWeightCenter pendulum
        angleDerivative = -pendulum.initialAngle * sin (sqrt (gravitationalConstant/d) * timeSeconds)
    in
        compare angleDerivative 0

shouldHaveEffects : Pendulum -> Float -> Float -> Bool
shouldHaveEffects pendulum currentAngle timeSeconds =
    let direction = movementDirection pendulum timeSeconds
        angleSign = compare currentAngle 0
    in
        abs currentAngle < 0.2 && (angleSign == EQ || angleSign == direction)

circleFill : Bool -> String
circleFill marked =
    if marked then "blue"
    else "transparent"
