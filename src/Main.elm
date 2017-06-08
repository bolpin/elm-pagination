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
    , sortDirection : SortDirection
    }


type alias Pet =
    { name : String
    , meals : Int
    }


type SortDirection
    = Ascending
    | Descending


init : ( Model, Cmd Msg )
init =
    { pets = initPets
    , numPerPage = 5
    , page = 1
    , filter = ""
    , sortField = "name"
    , sortDirection = Descending
    }
        ! [ Cmd.none ]


initPets : List Pet
initPets =
    [ { name = "Chief", meals = 4 }
    , { name = "Cinco", meals = 6 }
    , { name = "Deacon", meals = 55 }
    , { name = "Friendly", meals = 683 }
    , { name = "Frog", meals = 34 }
    , { name = "Luca", meals = 34 }
    , { name = "Lucy", meals = 311 }
    , { name = "Norberta", meals = 7 }
    , { name = "Pepper", meals = 3 }
    , { name = "Smokey", meals = 73 }
    , { name = "Toad", meals = 32 }
    ]


type Msg
    = UpdateFilter String
    | SortBy String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFilter newFilter ->
            { model | filter = newFilter } ! []

        SortBy field ->
            case ( field, model.sortDirection ) of
                ( "name", Ascending ) ->
                    { model | sortField = "name", sortDirection = Descending } ! []

                ( "name", Descending ) ->
                    { model | sortField = "name", sortDirection = Ascending } ! []

                ( "meals", Ascending ) ->
                    { model | sortField = "meals", sortDirection = Descending } ! []

                ( "meals", Descending ) ->
                    { model | sortField = "meals", sortDirection = Ascending } ! []

                ( _, _ ) ->
                    { model | sortField = "meals", sortDirection = Ascending } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewControls model
        , viewPage model
        , viewPaginator model
        , viewFooter model
        ]


sortDirString : SortDirection -> String
sortDirString sortDirection =
    case sortDirection of
        Ascending ->
            "⬇"

        _ ->
            "⬆"


viewFooter : Model -> Html Msg
viewFooter model =
    footer []
        [ ul []
            [ li [] [ text model.sortField ]
            , li [] [ text (toString model.page) ]
            ]
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
        (viewRows model)


viewRows : Model -> List (Html Msg)
viewRows model =
    let
        filteredPets =
            filterPets model.filter model.pets

        sortedFilteredPets =
            case ( model.sortField, model.sortDirection ) of
                ( "name", Ascending ) ->
                    List.sortBy .name filteredPets

                ( "name", Descending ) ->
                    List.reverse <| List.sortBy .name filteredPets

                ( "meals", Ascending ) ->
                    List.sortBy .meals filteredPets

                ( _, _ ) ->
                    List.reverse <| List.sortBy .meals filteredPets
    in
        (viewTableHeader model.sortField model.sortDirection) :: List.map viewRow sortedFilteredPets


sortPets : List Pet -> String -> SortDirection -> List Pet
sortPets pets sortField sortDirection =
    pets


filterPets : String -> List Pet -> List Pet
filterPets filter pets =
    let
        pattern =
            Regex.caseInsensitive (Regex.regex filter)

        nameMatches pet =
            Regex.contains pattern pet.name
    in
        List.filter nameMatches pets


onePagePets : Int -> Int -> List Pet -> List Pet
onePagePets numPerPage page pets =
    pets


viewTableHeader : String -> SortDirection -> Html Msg
viewTableHeader sortField sortDirection =
    let
        nameHeader =
            case ( sortField, sortDirection ) of
                ( "name", _ ) ->
                    "name " ++ (sortDirString sortDirection)

                ( _, _ ) ->
                    "name"

        mealsHeader =
            case ( sortField, sortDirection ) of
                ( "meals", _ ) ->
                    "meals " ++ (sortDirString sortDirection)

                ( _, _ ) ->
                    "meals"
    in
        tr []
            [ th []
                [ a
                    [ href "#"
                    , onClick (SortBy "name")
                    ]
                    [ text nameHeader ]
                ]
            , th []
                [ a
                    [ href "#"
                    , onClick (SortBy "meals")
                    ]
                    [ text mealsHeader ]
                ]
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
