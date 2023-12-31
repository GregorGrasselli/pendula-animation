port module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html
import Html.Events exposing (onClick)
import Html.Attributes as HA
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List
import Pendulum exposing (..)
import Messages exposing (..)
import String
import Background


type alias Model = { activeTime: Float
                   , play: Bool
                   , pendula: List(Pendulum)
                   }

initialPendula : List(Pendulum)
initialPendula = buildPendula 10 -0.8 [50, 100, 200, 400, 450] ["D3", "F#3", "A3", "C4", "B3"]

init : () -> (Model, Cmd Msg)
init _ = (Model 0 False initialPendula, Cmd.none)

type alias Message = { kind: String
                     , tones: List(String)
                     }

port sendMessage : Message -> Cmd msg

onNewFrame : Model -> Float -> (Model, Cmd Msg)
onNewFrame model timeDelta =
    if model.play then
        let newTime = model.activeTime + timeDelta / 1000
            (updatedPendula, tonesToPlay) = updatePendula model.pendula newTime
            command = if tonesToPlay == [] then Cmd.none else sendMessage (Message "play" tonesToPlay)
        in
            ({ model | pendula = updatedPendula, activeTime = newTime }, command)
    else
        (model, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewFrame delta -> onNewFrame model delta
        TogglePlay -> ({ model | play = not model.play}, sendMessage (Message "start" []))

view : Model -> Html.Html Msg
view model =
    let
        width_ = 700
        widthStr = String.fromFloat width_
        height_ = 600
        heightStr = String.fromFloat height_
        topBallRadius = 10
    in
        Html.div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "justify-content" "center"
            , HA.style "max-width" "700px"
            ]
            [ svg
              [ width "100%"
              , viewBox (String.join " " ["0", "-5", widthStr, heightStr])
              ]
              (
               (Background.drawBackground width_ height_ (width_ / 2) topBallRadius)
               ++
               (List.map (drawPendulum (width_/2, topBallRadius)) model.pendula)
              )
            , Html.button [onClick TogglePlay, HA.style "margin-top" "5px"]
                [Html.text (if not model.play then "Play" else "Pause")]
            ]

subscriptions : Model -> Sub Msg
subscriptions _ = onAnimationFrameDelta NewFrame

main : Program () Model Msg
main =
    Browser.element { init = init
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                    }
