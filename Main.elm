module Main exposing (Color(..), DrawPath, Model, MouseState(..), Msg(..), Position, colorPicker, init, main, offsetPosition, pointToString, pointsToSvgLine, subscriptions, update, view)

import Browser exposing (document)
import Browser.Events as Events
import Config exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing (..)
import Svg exposing (circle, polyline, rect, svg)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import VirtualDom exposing (..)


main : Program Json.Value Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { text : String
    , mousePos : Position
    , picture : List DrawPath
    , currentLine : DrawPath
    , currentColor : Color
    , isDrawing : Bool
    }


type alias DrawPath =
    { color : Color
    , points : List ( Float, Float )
    }


type alias Position =
    { x : Int
    , y : Int
    }


type Msg
    = NewColor Color
    | MousePos Position
    | MouseClicked MouseState
    | ResetPanel


type MouseState
    = Up
    | Down


type Color
    = Black
    | Red
    | Blue


init : ( Model, Cmd Msg )
init =
    ( { text = "Init "
      , mousePos = Position 0 0
      , picture = []
      , currentLine =
            { color = Black
            , points = []
            }
      , isDrawing = False
      , currentColor = Black
      }
    , Cmd.none
    )


pointToString : ( Float, Float ) -> String
pointToString p =
    let
        x =
            String.fromFloat <| first p

        y =
            String.fromFloat <| second p
    in
    x ++ "," ++ y


pointsToSvgLine : DrawPath -> Svg.Svg msg
pointsToSvgLine { color, points } =
    let
        x =
            List.map pointToString points
                |> String.join " "

        polyColor =
            case color of
                Red ->
                    "red"

                Blue ->
                    "blue"

                Black ->
                    "black"
    in
    polyline
        [ fill "none"
        , stroke polyColor
        , Svg.Attributes.points x
        ]
        []


view : Model -> Browser.Document Msg
view model =
    let
        x =
            toFloat <| model.mousePos.x - round (cWidth / 2)

        y =
            toFloat <| -model.mousePos.y + round (cHeight / 2)

        myViewBox =
            viewBox <| "0 0 " ++ String.fromFloat cWidth ++ " " ++ String.fromFloat cHeight

        mouseX =
            String.fromInt model.mousePos.x

        mouseY =
            String.fromInt model.mousePos.y

        lineToDraw =
            pointsToSvgLine model.currentLine
    in
    { title = "Tegn i vei"
    , body =
        [ div
            []
            [ Html.text (model.text ++ " " ++ Debug.toString model.isDrawing) ]
        , div
            [ Html.Attributes.style "border" "1px solid black"
            ]
            [ svg
                [ myViewBox
                , Svg.Attributes.width <| String.fromInt cWidth ++ "px"
                , Svg.Attributes.height <| String.fromInt cHeight ++ "px"
                , Html.Attributes.style "border" "1px solid black"

                --VirtualDom.on "click" (VirtualDom.Normal Json.succeed)
                ]
                [ circle
                    [ cx mouseX
                    , cy mouseY
                    , r "10"
                    , fill "#0B79CE"
                    ]
                    []
                , lineToDraw
                ]
            ]
        , colorPicker
        , Html.button
            [ onClick ResetPanel
            ]
            [ Html.text "Clear drawing" ]
        ]
    }


colorPicker : Html Msg
colorPicker =
    div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "width" "100%"
        ]
        [ div
            [ onClick <| NewColor Red
            , Html.Attributes.style "background-color" "red"
            , Html.Attributes.style "flex-basis" "100%"
            ]
            [ Html.text "red" ]
        , div
            [ onClick <| NewColor Blue
            , Html.Attributes.style "background-color" "blue"
            , Html.Attributes.style "flex-basis" "100%"
            ]
            [ Html.text "blue" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewColor Red ->
            ( { model | text = "red", currentColor = Red }, Cmd.none )

        NewColor Blue ->
            ( { model | text = "blue", currentColor = Blue }, Cmd.none )

        NewColor Black ->
            ( { model | text = "black", currentColor = Black }, Cmd.none )

        MousePos pos ->
            case model.isDrawing of
                True ->
                    let
                        pointsSoFar =
                            model.currentLine.points

                        xPos =
                            toFloat pos.x

                        yPos =
                            toFloat pos.y

                        picture =
                            model.currentLine

                        newCurrentLine =
                            { picture
                                | points = ( xPos, yPos ) :: pointsSoFar
                                , color = model.currentColor
                            }
                    in
                    ( { model
                        | mousePos = pos
                        , currentLine = newCurrentLine
                      }
                    , Cmd.none
                    )

                False ->
                    ( model, Cmd.none )

        MouseClicked state ->
            case state of
                Up ->
                    ( { model | isDrawing = False }, Cmd.none )

                Down ->
                    ( { model | isDrawing = True }, Cmd.none )

        ResetPanel ->
            init


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onMouseDown (Json.succeed <| MouseClicked Down)
        , Events.onMouseUp (Json.succeed <| MouseClicked Up)
        , Events.onMouseMove (Json.map MousePos offsetPosition)
        ]


offsetPosition : Json.Decoder Position
offsetPosition =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)
