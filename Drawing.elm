module Drawing exposing (init, pointsToSvgLine, update)

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
                    ( { model
                        | mousePos = pos
                      }
                    , Cmd.none
                    )

        MouseClicked state ->
            case state of
                Up ->
                    let
                        newPicture =
                            model.currentLine :: model.picture
                    in
                    ( { model
                        | isDrawing = False
                        , picture = newPicture
                        , currentLine =
                            { color = model.currentColor
                            , points = []
                            }
                      }
                    , Cmd.none
                    )

                Down ->
                    ( { model | isDrawing = True }, Cmd.none )

        ChooseCircle ->
            ( { model
                | text = "circlemode"
                , drawingMode = CircleMode
              }
            , Cmd.none
            )

        ChooseFreeDraw ->
            ( { model
                | text = "freedraw lemode"
                , drawingMode = FreeDraw
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
