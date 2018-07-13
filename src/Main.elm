module Main exposing (..)

import Color exposing (..)
import Html exposing (Html)
import Style exposing (Style)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (..)
import Crop exposing (..)
import Rotation exposing (..)


---- MODEL ----


type alias Model =
    { width : Int
    , height : Int
    , parcels : List Parcel
    , selected : Maybe Parcel
    , highlighted : Maybe Crop
    }


type alias Parcel =
    { crops : List Crop
    , boundingBox : BoundingBox
    }


type alias BoundingBox =
    { topLeft : ( Int, Int )
    , width : Int
    , height : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { width = 25
      , height = 22
      , selected = Nothing
      , highlighted = Nothing
      , parcels =
            [ { crops = [ Tomato ]
              , boundingBox =
                    { topLeft = ( 0, 0 )
                    , width = 25
                    , height = 1
                    }
              }
            , { crops = [ Beetroot ]
              , boundingBox =
                    { topLeft = ( 0, 1 )
                    , width = 4
                    , height = 3
                    }
              }
            , { crops = [ Salad, Radish ]
              , boundingBox =
                    { topLeft = ( 5, 1 )
                    , width = 4
                    , height = 3
                    }
              }
            , { crops = [ Carot, Radish, Scallion ]
              , boundingBox =
                    { topLeft = ( 10, 1 )
                    , width = 4
                    , height = 3
                    }
              }
            , { crops = [ Garlic ]
              , boundingBox =
                    { topLeft = ( 15, 1 )
                    , width = 2
                    , height = 3
                    }
              }
            , { crops = [ Salad ]
              , boundingBox =
                    { topLeft = ( 20, 1 )
                    , width = 4
                    , height = 3
                    }
              }
            , { crops = [ Salad ]
              , boundingBox =
                    { topLeft = ( 20, 6 )
                    , width = 4
                    , height = 4
                    }
              }
            , { crops = [ Salad ]
              , boundingBox =
                    { topLeft = ( 20, 12 )
                    , width = 4
                    , height = 4
                    }
              }
            , { crops = [ Shallot ]
              , boundingBox =
                    { topLeft = ( 15, 14 )
                    , width = 4
                    , height = 2
                    }
              }
            , { crops = [ Tomato, Radish ]
              , boundingBox =
                    { topLeft = ( 0, 6 )
                    , width = 4
                    , height = 4
                    }
              }
            , { crops = [ Tomato ]
              , boundingBox =
                    { topLeft = ( 5, 6 )
                    , width = 4
                    , height = 4
                    }
              }
            , { crops = [ Tomato ]
              , boundingBox =
                    { topLeft = ( 10, 6 )
                    , width = 4
                    , height = 4
                    }
              }
            , { crops = [ Squash, Bean ]
              , boundingBox =
                    { topLeft = ( 0, 12 )
                    , width = 9
                    , height = 10
                    }
              }
            , { crops = [ Bean ]
              , boundingBox =
                    { topLeft = ( 11, 17 )
                    , width = 2
                    , height = 5
                    }
              }
            , { crops = [ Peas ]
              , boundingBox =
                    { topLeft = ( 24, 0 )
                    , width = 1
                    , height = 22
                    }
              }
            ]
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SelectParcel Parcel
    | ToggleHighlight Crop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectParcel parcel ->
            ( { model | selected = Just parcel }, Cmd.none )

        ToggleHighlight crop ->
            ( { model
                | highlighted =
                    if Just crop == model.highlighted then
                        Nothing
                    else
                        Just crop
              }
            , Cmd.none
            )



---- VIEW ----


type Styles
    = Main
    | Grid
    | Cell
    | ParcelStyle
    | SidePanel
    | H1
    | Text
    | Table
    | TableHeader
    | CropIcon
    | Successor


type Variations
    = Selected
    | Highlighted


stylesheet =
    Style.styleSheet
        [ Style.style Main
            [ Font.typeface [ Font.sansSerif ]
            , Font.size 16
            , Color.text (greyscale 0.6)
            ]
        , Style.style H1
            [ Font.size 36
            , Font.weight 600
            , Color.text black
            ]
        , Style.style Grid
            [ Color.border darkCharcoal
            , Border.all 1
            , Border.right 2
            , Border.bottom 2
            ]
        , Style.style Cell
            [ Color.border darkGrey
            , Border.all 1
            ]
        , Style.style ParcelStyle
            [ Color.border black
            , Border.all 4
            , Style.prop "z-index" "10"
            , Color.background white
            , Style.variation
                Selected
                [ Color.border blue
                , Color.background lightBlue
                ]
            , Style.variation
                Highlighted
                [ Color.border red
                , Color.background lightRed
                ]
            ]
        , Style.style TableHeader
            [ Color.text (greyscale 0.7)
            , Font.weight 600
            , Font.size 15
            ]
        , Style.style Successor
            [ Font.size 18
            , Style.prop "cursor" "pointer"
            , Style.variation Highlighted [ Color.text red ]
            ]
        ]


viewEmptyCell row column =
    cell
        { start = ( row, column )
        , width = 1
        , height = 1
        , content =
            el Cell [] empty
        }


viewParcelCell highlighted selected ({ boundingBox } as parcel) =
    cell
        { start = boundingBox.topLeft
        , width = boundingBox.width
        , height = boundingBox.height
        , content =
            el
                ParcelStyle
                [ onClick (SelectParcel parcel)
                , vary Selected (selected == Just parcel)
                , vary Highlighted (List.member highlighted (List.map (\c -> Just c) parcel.crops))
                ]
                empty
        }


viewSidePanel highlighted selected =
    selected
        |> Maybe.map (viewSelection highlighted)
        |> Maybe.withDefault empty


viewSelection highlighted parcel =
    column
        SidePanel
        [ paddingTop 36, paddingLeft 48 ]
        [ h1
            H1
            [ paddingBottom 24 ]
            (parcel.crops |> List.map displayCropName |> String.join ", " |> text)
        , table
            Table
            [ spacing 16 ]
            [ [ el TableHeader [] (text "Familles")
              , el TableHeader [] (text "Catégories alimentaires")
              , el TableHeader [] (text "Successeurs potentiels")
              ]
            , [ parcel.crops |> List.map (family >> toString) |> String.join ", " |> text
              , parcel.crops |> List.map (category >> displayCategoryName) |> String.join ", " |> text
              , parcel.crops
                    |> successors
                    |> List.map (viewSuccessor highlighted)
                    |> column Text [ spacing 16 ]
              ]
            ]
        ]


viewSuccessor highlighted crop =
    row
        Successor
        [ verticalCenter
        , spacing 12
        , onClick (ToggleHighlight crop)
        , vary Highlighted (highlighted == Just crop)
        ]
        [ cropIcon crop
        , crop |> displayCropName |> text
        ]


cropIcon crop =
    case crop of
        Tomato ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/069-tomato.svg" }

        Beetroot ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/003-turnip-1.svg" }

        Salad ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/022-leaf.svg" }

        Radish ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/074-turnip.svg" }

        Squash ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/008-pumpkin.svg" }

        Bean ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/066-beans.svg" }

        Peas ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/013-peas.svg" }

        Carot ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/039-carrot.svg" }

        Scallion ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/016-onion.svg" }

        Garlic ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/028-garlic-2.svg" }

        Shallot ->
            decorativeImage CropIcon [ width (px 25), height (px 25) ] { src = "/icons/016-onion.svg" }


view : Model -> Html Msg
view { width, height, parcels, selected, highlighted } =
    layout stylesheet <|
        row Main
            [ padding 36 ]
            [ grid Grid
                []
                { columns = List.repeat width (px 25)
                , rows = List.repeat height (px 25)
                , cells =
                    List.range 0 (width - 1)
                        |> List.concatMap (\i -> List.range 0 (height - 1) |> List.map (\j -> viewEmptyCell i j))
                        |> List.append (List.map (viewParcelCell highlighted selected) parcels)
                }
            , viewSidePanel highlighted selected
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
