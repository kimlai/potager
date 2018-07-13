module Rotation exposing (..)

import Crop exposing (..)


successors : List Crop -> List Crop
successors crops =
    let
        availableFamilies =
            [ Apiaceae
            , Asteraceae
            , Brassicaceae
            , Chenopodiaceae
            , Cucurbitaceae
            , Fabaceae
            , Alliaceae
            , Solanaceae
            ]
                |> List.filter (\f -> not (List.member f (List.map family crops)))

        availableCategories =
            [ Leaf
            , Fruit
            , Root
            , Seed
            , Bulb
            ]
                |> List.filter (\f -> not (List.member f (List.map category crops)))
    in
        Crop.allCrops
            |> List.filter (\c -> List.member (family c) availableFamilies)
            |> List.filter (\c -> List.member (category c) availableCategories)
