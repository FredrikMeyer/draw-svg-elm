module Main exposing (main)

import Browser exposing (document)
import Drawing exposing (..)
import Json.Decode as Json exposing (Value)



-- todo bezier


main : Program Json.Value Model Msg
main =
    document
        { init = always init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []
