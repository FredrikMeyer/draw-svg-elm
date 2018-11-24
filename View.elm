module View exposing (view)

import Browser exposing (document)
import Config exposing (..)
import Drawing exposing (modelToSvg)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json exposing (..)
import Model exposing (..)
import Svg exposing (circle, polyline, rect, svg)
import Svg.Attributes exposing (..)


toPicture : Mode -> List DrawingElement -> List DrawingElement
toPicture mode list =
    case mode of
        FreeDraw m ->
            FreeHandElement m.currentLine :: list

        CircleMode _ ->
            list


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

        linesToDraw =
            List.map modelToSvg <| toPicture model.drawingMode model.picture
    in
    { title = "Tegn i vei"
    , body =
        [ div
            [ Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "text-align" "center"
            ]
            [ div
                [ --Html.Attributes.style "width" <| String.fromInt cWidth ++ "px"
                  Html.Attributes.style "display" "inline-block"
                ]
                [ div []
                    [ Html.h1 [] [ Html.text "SVG Drawer!" ]
                    ]
                , div
                    []
                    [ Html.text model.text ]
                , div
                    [ Html.Attributes.style "border" "1px solid red"
                    , Html.Attributes.style "display" "flex"
                    ]
                    [ div []
                        menuItems
                    , drawingBox myViewBox mouseX mouseY linesToDraw
                    ]
                , colorPicker
                , Html.button
                    [ onClick ResetPanel ]
                    [ Html.text "Clear drawing" ]
                , Html.button
                    [ onClick Undo ]
                    [ Html.text "Undo last line" ]
                ]
            ]
        ]
    }


menuItems =
    [ div
        [ onClick ChooseCircle
        , Html.Attributes.style "font-size" "40px"
        ]
        [ Html.text "O"
        ]
    , div
        [ onClick ChooseFreeDraw
        , Html.Attributes.style "font-size" "40px"
        ]
        [ Html.text "/"
        ]
    ]


drawingBox : Svg.Attribute Msg -> String -> String -> List (Svg.Svg Msg) -> Svg.Svg Msg
drawingBox myViewBox mouseX mouseY linesToDraw =
    svg
        [ myViewBox
        , Svg.Attributes.width <| String.fromInt cWidth ++ "px"
        , Svg.Attributes.height <| String.fromInt cHeight ++ "px"
        , Html.Attributes.style "border" "1px solid black"
        , Html.Events.on "mousemove" (Json.map MousePos offsetPosition)
        , Html.Events.onMouseDown <| MouseClicked Down
        , Html.Events.onMouseUp <| MouseClicked Up
        ]
        (circle
            [ cx mouseX
            , cy mouseY
            , r "10"
            , fill "#0B79CE"
            ]
            []
            :: linesToDraw
        )


colorPicker : Html Msg
colorPicker =
    div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "width" "100%"
        , Html.Attributes.style "text-align" "center"
        ]
        [ div
            [ onClick <| NewColor Red
            , Html.Attributes.style "background-color" "red"
            , Html.Attributes.style "flex-basis" "100%"
            ]
            [ Html.text "Red" ]
        , div
            [ onClick <| NewColor Blue
            , Html.Attributes.style "background-color" "blue"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "flex-basis" "100%"
            ]
            [ Html.text "Blue" ]
        , div
            [ onClick <| NewColor Black
            , Html.Attributes.style "background-color" "black"
            , Html.Attributes.style "color" "white"
            , Html.Attributes.style "flex-basis" "100%"
            ]
            [ Html.text "Black"
            ]
        ]


offsetPosition : Json.Decoder Position
offsetPosition =
    map2 Position
        (field "offsetX" int)
        (field "offsetY" int)
