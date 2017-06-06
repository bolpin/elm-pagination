port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { pets : List Pet
    , numPerPage : Int
    , page : Int
    , filter : String
    , sortField : String
    , sortDirection : String
    }


type alias Pet =
    { name : String
    , meals : Int
    }


type SortDirection
    = Asc
    | Desc


init : ( Model, Cmd Msg )
init =
    { pets = initPets
    , numPerPage = 5
    , page = 1
    , filter = ""
    , sortField = "name"
    , sortDirection = "asc"
    }
        ! [ Cmd.none ]


initPets : List Pet
initPets =
    [ { name = "Pepper", meals = 3 }
    , { name = "Deacon", meals = 55 }
    , { name = "Norberta", meals = 7 }
    , { name = "Chief", meals = 4 }
    , { name = "Cinco", meals = 6 }
    , { name = "Luca", meals = 34 }
    , { name = "Smokey", meals = 73 }
    , { name = "Frog", meals = 34 }
    , { name = "Toad", meals = 32 }
    , { name = "Lucy", meals = 311 }
    , { name = "Friendly", meals = 683 }
    ]


type Msg
    = UpdateFilter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFilter newFilter ->
            { model | filter = newFilter } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewControls model
        , viewPage model
        , viewPaginator model
        ]


viewControls : Model -> Html Msg
viewControls model =
    header []
        [ input
            [ type_ "text"
            , placeholder "Filter"
            , autofocus True
            , value model.filter
            , onInput UpdateFilter
            ]
            []
        ]


viewPage : Model -> Html Msg
viewPage model =
    table []
        (viewRows model.pets model.filter)


viewRows : List Pet -> String -> List (Html Msg)
viewRows pets filter =
    let
        pattern =
            Regex.caseInsensitive (Regex.regex filter)

        nameMatches pet =
            Regex.contains pattern pet.name
    in
        viewTableHeader :: List.map viewRow (List.filter nameMatches pets)


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th [] [ text "name" ]
        , th [] [ text "meals" ]
        ]


viewRow : Pet -> Html Msg
viewRow pet =
    tr []
        [ td [] [ text pet.name ]
        , td [] [ text (toString pet.meals) ]
        ]


viewPaginator : Model -> Html Msg
viewPaginator model =
    div [] [ text "paginator goes HERE" ]
