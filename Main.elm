module Main exposing (main)

import Browser exposing (document)
import Drawing exposing (update)
import Json.Decode as Json exposing (Value)
import Model exposing (Model, Msg, init)
import View exposing (view)



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
