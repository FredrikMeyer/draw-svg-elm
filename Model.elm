module Model exposing (Circle, Color(..), DrawPath, DrawingElement(..), Mode(..), Model, MouseState(..), Msg(..), Position, init)


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
      , drawingMode = FreeDraw
      }
    , Cmd.none
    )


type alias Model =
    { text : String
    , mousePos : Position
    , picture : List DrawPath
    , currentLine : DrawPath
    , currentColor : Color
    , isDrawing : Bool
    , drawingMode : Mode
    }


type Mode
    = FreeDraw
    | CircleMode


type DrawingElement
    = List DrawPath
    | Element Circle


type alias DrawPath =
    { color : Color
    , points : List ( Float, Float )
    }


type alias Circle =
    { color : Color
    , x : Int
    , y : Int
    , r : Int
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
