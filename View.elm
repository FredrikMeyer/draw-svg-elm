module View exposing (view)

import Drawing exposing (Model)


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
            List.map pointsToSvgLine (model.currentLine :: model.picture)
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
                    [ Html.h1 [] [ Html.text "SVG Drawer" ]
                    ]
                , div
                    []
                    [ Html.text model.text ]
                , div
                    [ Html.Attributes.style "border" "1px solid red"
                    , Html.Attributes.style "display" "flex"
                    ]
                    [ div []
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
