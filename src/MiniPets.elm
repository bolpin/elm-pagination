-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html


module Main exposing (..)

import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)


main =
    beginnerProgram { model = init, view = view, update = update }


init =
    { count = 0, level = 0 }


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text ((toString model.count) ++ (noun model.count) ++ excite model.level) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Reset ] [ text "reset" ]
        , button [ onClick Excite ] [ text "make exciting" ]
        ]


type Msg
    = Increment
    | Decrement
    | Reset
    | Excite


noun numPets =
    case abs (numPets) of
        1 ->
            " Pet"

        _ ->
            " Pets"


excite level =
    String.repeat level "!!!"


update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        Reset ->
            { model | count = 0, level = 0 }

        Excite ->
            { model | level = model.level + 1 }
