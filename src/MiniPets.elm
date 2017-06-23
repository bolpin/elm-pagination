module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, style, disabled, placeholder, autofocus, type_, value)
import Html.Events exposing (onInput, onClick)
import Regex exposing (caseInsensitive, regex, contains)


-- Boilerplate wiring


main =
    beginnerProgram
        { model = init
        , view = view
        , update = update
        }



-- Model


init =
    { count = 0
    , level = 0
    , noun = "Baby Duck"
    }



-- View


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div []
            [ text
                ((toString model.count)
                    ++ " "
                    ++ (pluralize model.noun model.count)
                    ++ excite model.level
                )
            ]
        , button [ onClick Increment ]
            [ text "+" ]
        , input
            [ type_ "text"
            , placeholder "Filter by Name"
            , value model.noun
            , onInput UpdateNoun
            , autofocus True
            ]
            []
        , button [ onClick Reset ] [ text "reset" ]
        , button [ onClick Excite ] [ text "make exciting" ]
        ]


pluralize noun count =
    case abs (count) of
        1 ->
            noun

        _ ->
            noun ++ "s"


excite level =
    String.repeat level "!!!"



-- Update


type Msg
    = Increment
    | Decrement
    | Reset
    | Excite
    | UpdateNoun String


update msg model =
    case msg of
        UpdateNoun newNoun ->
            { model | noun = newNoun }

        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        Reset ->
            { model | count = 0, level = 0 }

        Excite ->
            { model | level = model.level + 1 }
