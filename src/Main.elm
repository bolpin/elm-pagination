port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { invoices : List Invoice
    , numPerPage : Int
    , page : Int
    , filter : String
    , sortField : String
    , sortDirection : String
    }


type alias Invoice =
    { name : String
    , amount : Int
    }


type SortDirection
    = Asc
    | Desc


init : ( Model, Cmd Msg )
init =
    { invoices = initInvoices
    , numPerPage = 5
    , page = 1
    , filter = ""
    , sortField = "name"
    , sortDirection = "asc"
    }
        ! [ Cmd.none ]


initInvoices : List Invoice
initInvoices =
    [ { name = "Pepper", amount = 3 }
    , { name = "Deacon", amount = 55 }
    , { name = "Norberta", amount = 7 }
    , { name = "Chief", amount = 4 }
    , { name = "Cinco", amount = 6 }
    , { name = "Luca", amount = 34 }
    , { name = "Smokey", amount = 73 }
    , { name = "Frog", amount = 34 }
    , { name = "Toad", amount = 32 }
    , { name = "Lucy", amount = 311 }
    , { name = "Friendly", amount = 683 }
    ]


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ renderControls model
        , renderPage model
        , renderPaginator model
        ]


renderControls : Model -> Html Msg
renderControls model =
    div [] [ text "controls go HERE" ]


renderPage : Model -> Html Msg
renderPage model =
    table []
        (renderRows model.invoices)


renderRows : List Invoice -> List (Html Msg)
renderRows invoices =
    viewTableHeader :: List.map renderRow invoices


viewTableHeader : Html Msg
viewTableHeader =
    tr []
        [ th [] [ text "name" ]
        , th [] [ text "amount" ]
        ]


renderRow : Invoice -> Html Msg
renderRow invoice =
    tr []
        [ td [] [ text invoice.name ]
        , td [] [ text (toString invoice.amount) ]
        ]


renderPaginator : Model -> Html Msg
renderPaginator model =
    div [] [ text "paginator goes HERE" ]
