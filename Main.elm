module Main exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Element exposing (toHtml)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Mouse exposing (Position)


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
    , isDrawing : Bool
    }


width =
    600


height =
    600


type Msg
    = NewColor Color
    | MousePos Position
    | MouseClicked MouseState


type MouseState
    = Up
    | Down


type Color
    = Red
    | Blue


view : Model -> Html Msg
view model =
    let
        x =
            toFloat <| model.mousePos.x - round (width / 2)

        y =
            toFloat <| -model.mousePos.y + round (height / 2)

        lineToDraw =
            traced (solid black)
                (path model.picture)
    in
        div []
            [ div [] [ Html.text (model.text ++ " " ++ toString model.isDrawing) ]
            , div
                [ style
                    [ ( "border", "1px solid black" )
                    ]
                ]
                [ toHtml
                    (collage
                        -- gå over til SVG
                        width
                        height
                        [ filled black (rect 10 10)
                            |> moveX x
                            |> moveY y
                        , lineToDraw
                        ]
                    )
                ]
            , div
                [ id "colorPickerRow"
                ]
                colorPicker
            ]


colorPicker : List (Html Msg)
colorPicker =
    [ div
        [ onClick <| NewColor Red
        , style
            [ ( "background-color", "red" )
            , ( "flex", "1 1 auto" )
            ]
        ]
        [ Html.text "red" ]
    , div
        [ onClick <| NewColor Blue
        , style
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
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewColor Red ->
            ( { model | text = "red" }, Cmd.none )

        NewColor Blue ->
            ( { model | text = "blue" }, Cmd.none )

        MousePos pos ->
            case model.isDrawing of
                True ->
                    let
                        pointsSoFar =
                            model.picture

                        xPos =
                            toFloat <| -(round (width / 2)) + pos.x

                        yPos =
                            toFloat <| -pos.y + round (height / 2)
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
        [ Mouse.moves (\p -> MousePos p)
        , Mouse.downs (\x -> MouseClicked Down)
        , Mouse.ups (\x -> MouseClicked Up)
        ]
