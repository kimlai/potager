module Main exposing (..)

import Html exposing (Html)
import Style exposing (Style)
import Style.Border as Border
import Element exposing (..)
import Element.Attributes exposing (..)


---- MODEL ----


type alias Model =
    { width : Int
    , height : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { width = 10, height = 8 }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


type Styles
    = Main
    | Grid
    | Cell


stylesheet =
    Style.styleSheet
        [ Style.style Grid
            [ Border.all 1
            , Border.right 2
            , Border.bottom 2
            ]
        , Style.style Cell
            [ Border.all 1 ]
        ]


viewEmptyCell row column =
    cell
        { start = ( row, column )
        , width = 1
        , height = 1
        , content =
            el Cell [] empty
        }


view : Model -> Html Msg
view { width, height } =
    layout stylesheet <|
        el Main
            [ center, spacing 100 ]
            (grid Grid
                []
                { columns = List.repeat width (px 50)
                , rows = List.repeat height (px 50)
                , cells =
                    List.range 0 (width - 1)
                        |> List.concatMap (\i -> List.range 0 (height - 1) |> List.map (\j -> viewEmptyCell i j))
                }
            )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
