module Model exposing (Circle, Color(..), DrawPath, DrawingElement(..), Mode(..), Model, MouseState(..), Msg(..), Position, init)


init : ( Model, Cmd Msg )
init =
    ( { text = "Init "
      , mousePos = Position 0 0
      , picture = []
      , currentColor = Black
      , drawingMode =
            FreeDraw
                { currentLine =
                    { color = Black
                    , points = []
                    }
                , isDrawing = False
                }
      }
    , Cmd.none
    )


type alias Model =
    { text : String
    , mousePos : Position
    , picture : List DrawingElement
    , currentColor : Color
    , drawingMode : Mode
    }


type Mode
    = FreeDraw
        { currentLine : DrawPath
        , isDrawing : Bool
        }
    | CircleMode
        { center : Maybe ( Float, Float )
        }


type DrawingElement
    = FreeHandElement DrawPath
    | CircleElement Circle


type alias DrawPath =
    { color : Color
    , points : List ( Float, Float )
    }


type alias Circle =
    { color : Color
    , x : Float
    , y : Float
    , r : Float
    }


type alias Position =
    { x : Int
    , y : Int
    }


type Msg
    = NewColor Color
    | MousePos Position
    | MouseClicked MouseState
    | ChooseCircle
    | ChooseFreeDraw
    | ResetPanel
    | Undo


type MouseState
    = Up
    | Down


type Color
    = Black
    | Red
    | Blue
