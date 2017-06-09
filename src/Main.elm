port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Regex exposing (..)
import Array exposing (Array, slice, toList, fromList)


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
    , { name = "Rocky", meals = 6 }
    , { name = "Buster", meals = 0 }
    , { name = "Abe", meals = 36 }
    , { name = "Cinco", meals = 56 }
    , { name = "Princess", meals = 16 }
    , { name = "Spot", meals = 74 }
    , { name = "Lucky", meals = 1 }
    , { name = "Tenbrooks", meals = 6 }
    , { name = "Bear", meals = 9 }
    , { name = "Red", meals = 4 }
    , { name = "Julie", meals = 4 }
    , { name = "Mister Kitty", meals = 4 }
    , { name = "Blue", meals = 76 }
    , { name = "Lassie", meals = 14 }
    , { name = "Deacon", meals = 55 }
    , { name = "Friendly", meals = 683 }
    , { name = "Frog", meals = 34 }
    , { name = "Luca", meals = 34 }
    , { name = "Lucy", meals = 311 }
    , { name = "Norberta", meals = 7 }
    , { name = "Winston", meals = 433 }
    , { name = "Pepper", meals = 3 }
    , { name = "Smokey", meals = 73 }
    , { name = "Mason", meals = 88 }
    , { name = "Toad", meals = 32 }
    ]



-- UPDATE


type Msg
    = UpdateFilter String
    | SortBy String
    | GoToFirst
    | GoToPrev
    | GoToNext
    | GoToLast


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToFirst ->
            { model | page = 1 } ! []

        GoToPrev ->
            { model | page = (Basics.max 1 (model.page - 1)) } ! []

        GoToNext ->
            { model | page = (Basics.min (numPages model) (model.page + 1)) } ! []

        GoToLast ->
            { model | page = (numPages model) } ! []

        UpdateFilter newFilter ->
            { model | filter = newFilter } ! []

        SortBy newField ->
            let
                swapDirection =
                    case (model.sortDirection) of
                        Ascending ->
                            Descending

                        Descending ->
                            Ascending

                newDirection newSortField =
                    case (model.sortField == newSortField) of
                        True ->
                            swapDirection

                        False ->
                            model.sortDirection
            in
                { model | sortField = newField, sortDirection = (newDirection newField) } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "margin", "50px auto" ), ( "width", "200px" ) ] ]
        [ h1 [] [ text "List of Pets" ]
        , viewControls model
        , viewPage model
        , viewPaginator model
        ]


sortDirString : String -> String -> SortDirection -> String
sortDirString sortField field sortDirection =
    case ( (sortField == field), sortDirection ) of
        ( False, _ ) ->
            ""

        ( True, Ascending ) ->
            " ⬇"

        ( True, Descending ) ->
            " ⬆"


numPages : Model -> Int
numPages model =
    1 + (List.length model.pets // model.numPerPage)


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

        firstIndex =
            (1 - model.page) * model.numPerPage

        lastIndex =
            Basics.min (List.length model.pets) (firstIndex + model.numPerPage)

        -- paginatedPets =
        --     toList (slice firstIndex (firstIndex + model.numPerPage) (model.sortedFilteredPets))
    in
        (viewTableHeader model.sortField model.sortDirection) :: List.map viewRow sortedFilteredPets


filterPets : String -> List Pet -> List Pet
filterPets filter pets =
    let
        pattern =
            Regex.caseInsensitive (Regex.regex filter)

        nameMatches pet =
            Regex.contains pattern pet.name
    in
        List.filter nameMatches pets



-- onePagePets : Int -> Int -> List Pet -> List Pet
-- onePagePets numPerPage page pets =
--     pets


viewTableHeader : String -> SortDirection -> Html Msg
viewTableHeader sortField sortDirection =
    let
        nameHeader =
            "Name" ++ sortDirString sortField "name" sortDirection

        mealsHeader =
            "Meals" ++ sortDirString sortField "meals" sortDirection
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
    div []
        [ text
            ("page "
                ++ toString model.page
                ++ " of "
                ++ toString (numPages model)
            )
        , viewPaginatorControls model
        ]


viewPaginatorControls : Model -> Html Msg
viewPaginatorControls model =
    let
        enablePrev =
            model.page /= 1

        enableNext =
            model.page /= numPages model
    in
        div []
            [ a
                [ href "#"
                , onClick GoToFirst
                , style
                    [ ( "padding-right", "1em" )
                    , ( "cursor", "pointer" )
                    ]
                ]
                [ text "first" ]
            , a
                [ href "#"
                , onClick GoToPrev
                , style
                    [ ( "padding-right", "1em" )
                    , ( "cursor", "pointer" )
                    ]
                ]
                [ text "prev" ]
            , a
                [ href "#"
                , onClick GoToNext
                , style
                    [ ( "padding-right", "1em" )
                    , ( "cursor", "pointer" )
                    ]
                ]
                [ text "next" ]
            , a
                [ href "#"
                , onClick GoToLast
                , style
                    [ ( "padding-right", "1em" )
                    , ( "cursor", "pointer" )
                    ]
                ]
                [ text "last" ]
            ]
