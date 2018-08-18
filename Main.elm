module Main exposing (..)

--import Collage exposing (..)

import Color exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Mouse exposing (Position)
import Svg exposing (svg, rect, circle, polyline)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import DOM exposing (target, boundingClientRect)
import VirtualDom exposing (..)
import Json.Decode as Json exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { text : String
    , mousePos : Position
    , picture : List ( Float, Float )
    , color : Color
    , isDrawing : Bool
    }


cWidth =
    300


cHeight =
    300


type Msg
    = NewColor Color
    | MousePos Position
    | MouseClicked MouseState


type MouseState
    = Up
    | Down


type Color
    = Black
    | Red
    | Blue


pointToString : ( Float, Float ) -> String
pointToString p =
    let
        x =
            toString <| first p

        y =
            toString <| second p
    in
        x ++ "," ++ y


pointsToSvgLine : Color -> List ( Float, Float ) -> Svg.Svg msg
pointsToSvgLine c pts =
    let
        x =
            List.map pointToString pts
                |> String.join " "

        color =
            case c of
                Red ->
                    "red"

                Blue ->
                    "blue"

                Black ->
                    "black"
    in
        polyline
            [ fill "none"
            , stroke color
            , points x
            ]
            []


view : Model -> Html Msg
view model =
    let
        x =
            toFloat <| model.mousePos.x - round (cWidth / 2)

        y =
            toFloat <| -model.mousePos.y + round (cHeight / 2)

        myViewBox =
            viewBox <| "0 0 " ++ toString cWidth ++ " " ++ toString cHeight

        mouseX =
            toString model.mousePos.x

        mouseY =
            toString model.mousePos.y

        lineToDraw =
            pointsToSvgLine model.color model.picture
    in
        div []
            [ div [] [ Html.text (model.text ++ " " ++ toString model.isDrawing) ]
            , div
                [ Html.Attributes.style
                    [ ( "border", "1px solid black" )
                    ]
                ]
                [ svg
                    [ myViewBox
                    , Svg.Attributes.width <| toString cWidth ++ "px"
                    , Svg.Attributes.height <| toString cHeight ++ "px"
                    , Html.Attributes.style
                        [ ( "border", "1px solid black" )
                        ]
                    , VirtualDom.on "mousemove" (Json.map MousePos offsetPosition)
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
            , div
                [ Html.Attributes.id "colorPickerRow"
                ]
                colorPicker
            ]


offsetPosition : Json.Decoder Position
offsetPosition =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)


colorPicker : List (Html Msg)
colorPicker =
    [ div
        [ onClick <| NewColor Red
        , Html.Attributes.style
            [ ( "background-color", "red" )
            , ( "flex", "1 1 auto" )
            ]
        ]
        [ Html.text "red" ]
    , div
        [ onClick <| NewColor Blue
        , Html.Attributes.style
            [ ( "background-color", "blue" )
            , ( "flex", "1 1 auto" )
            ]
        ]
        [ Html.text "blue" ]
    ]


init : ( Model, Cmd Msg )
init =
    ( { text = "Init "
      , mousePos = Position 0 0
      , picture = []
      , isDrawing = False
      , color = Black
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewColor Red ->
            ( { model | text = "red", color = Red }, Cmd.none )

        NewColor Blue ->
            ( { model | text = "blue", color = Blue }, Cmd.none )

        NewColor Black ->
            ( { model | text = "black", color = Black }, Cmd.none )

        MousePos pos ->
            case model.isDrawing of
                True ->
                    let
                        pointsSoFar =
                            model.picture

                        xPos =
                            toFloat pos.x

                        yPos =
                            toFloat pos.y
                    in
                        ( { model
                            | mousePos = pos
                            , picture = ( xPos, yPos ) :: pointsSoFar
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs (\x -> MouseClicked Down)
        , Mouse.ups (\x -> MouseClicked Up)
        ]
