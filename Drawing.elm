module Drawing exposing (modelToSvg, update)

import Browser.Events as Events
import Config exposing (..)
import Json.Decode as Json exposing (..)
import Model exposing (..)
import Svg exposing (circle, polyline, rect, svg)
import Svg.Attributes exposing (..)
import Tuple exposing (first, second)
import VirtualDom exposing (..)


pointToString : ( Float, Float ) -> String
pointToString p =
    let
        x =
            String.fromFloat <| first p

        y =
            String.fromFloat <| second p
    in
    x ++ "," ++ y


modelToSvg : DrawingElement -> Svg.Svg msg
modelToSvg element =
    case element of
        FreeHandElement drawPath ->
            freeHandToSvg drawPath

        CircleElement c ->
            let
                x =
                    String.fromFloat c.x ++ "px"

                y =
                    String.fromFloat c.y ++ "px"

                r =
                    String.fromFloat c.r ++ "px"

                color =
                    colorToString c.color
            in
            circle
                [ fill "none"
                , stroke color
                , Svg.Attributes.cx x
                , Svg.Attributes.cy y
                , Svg.Attributes.r r
                ]
                []


freeHandToSvg : DrawPath -> Svg.Svg msg
freeHandToSvg { color, points } =
    let
        x =
            List.map pointToString points
                |> String.join " "

        polyColor =
            colorToString color
    in
    polyline
        [ fill "none"
        , stroke polyColor
        , Svg.Attributes.points x
        ]
        []


colorToString : Color -> String
colorToString color =
    case color of
        Red ->
            "red"

        Blue ->
            "blue"

        Black ->
            "black"


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance x y =
    let
        xx =
            first x

        xy =
            second x

        yy =
            second y

        yx =
            first y
    in
    sqrt <| (xx - yx) ^ 2 + (xy - yy) ^ 2


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
            case model.drawingMode of
                FreeDraw { currentLine, isDrawing } ->
                    case isDrawing of
                        True ->
                            let
                                pointsSoFar =
                                    currentLine.points

                                xPos =
                                    toFloat pos.x

                                yPos =
                                    toFloat pos.y

                                picture =
                                    currentLine

                                newCurrentLine =
                                    { picture
                                        | points = ( xPos, yPos ) :: pointsSoFar
                                        , color = model.currentColor
                                    }

                                updatedFreeDraw =
                                    FreeDraw
                                        { currentLine = newCurrentLine
                                        , isDrawing = isDrawing
                                        }
                            in
                            ( { model
                                | mousePos = pos
                                , drawingMode = updatedFreeDraw
                              }
                            , Cmd.none
                            )

                        False ->
                            ( { model
                                | mousePos = pos
                              }
                            , Cmd.none
                            )

                CircleMode _ ->
                    ( { model | mousePos = pos }
                    , Cmd.none
                    )

        MouseClicked state ->
            case model.drawingMode of
                FreeDraw { currentLine, isDrawing } ->
                    case state of
                        Up ->
                            let
                                newPicture =
                                    FreeHandElement currentLine :: model.picture
                            in
                            ( { model
                                | picture = newPicture
                                , drawingMode =
                                    FreeDraw
                                        { currentLine =
                                            { color = model.currentColor
                                            , points = []
                                            }
                                        , isDrawing = False
                                        }
                              }
                            , Cmd.none
                            )

                        Down ->
                            let
                                newMode =
                                    FreeDraw
                                        { currentLine = currentLine
                                        , isDrawing = True
                                        }
                            in
                            ( { model | drawingMode = newMode }, Cmd.none )

                CircleMode c ->
                    case state of
                        Up ->
                            let
                                x =
                                    toFloat model.mousePos.x

                                y =
                                    toFloat model.mousePos.y

                                center =
                                    Maybe.withDefault ( 0, 0 ) c.center

                                radius =
                                    distance ( x, y ) center

                                newPicture =
                                    CircleElement
                                        { color = model.currentColor
                                        , x = c.center |> Maybe.map first |> Maybe.withDefault 0
                                        , y = c.center |> Maybe.map second |> Maybe.withDefault 0
                                        , r = radius
                                        }
                                        :: model.picture
                            in
                            ( { model | picture = newPicture }, Cmd.none )

                        Down ->
                            let
                                mouseX =
                                    toFloat model.mousePos.x

                                mouseY =
                                    toFloat model.mousePos.y

                                newMode =
                                    CircleMode
                                        { center = Just ( mouseX, mouseY )
                                        }
                            in
                            ( { model | drawingMode = newMode }, Cmd.none )

        ChooseCircle ->
            ( { model
                | text = "circle mode"
                , drawingMode = CircleMode { center = Nothing }
              }
            , Cmd.none
            )

        ChooseFreeDraw ->
            ( { model
                | text = "freedraw"
                , drawingMode =
                    FreeDraw
                        { currentLine =
                            { color = model.currentColor
                            , points = []
                            }
                        , isDrawing = False
                        }
              }
            , Cmd.none
            )

        ResetPanel ->
            init

        Undo ->
            case model.picture of
                _ :: rest ->
                    ( { model
                        | picture = rest
                        , text = model.text
                      }
                    , Cmd.none
                    )

                [] ->
                    ( { model | picture = [] }, Cmd.none )
