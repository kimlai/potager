module Crop exposing (..)


type Crop
    = Tomato
    | Beetroot
    | Salad
    | Radish
    | Squash
    | Bean
    | Peas
    | Carot
    | Scallion
    | Garlic
    | Shallot


type Family
    = Apiaceae
    | Asteraceae
    | Brassicaceae
    | Chenopodiaceae
    | Cucurbitaceae
    | Fabaceae
    | Alliaceae
    | Solanaceae
    | Other


type Category
    = Leaf
    | Fruit
    | Root
    | Seed
    | Bulb


family crop =
    case crop of
        Tomato ->
            Solanaceae

        Beetroot ->
            Chenopodiaceae

        Salad ->
            Asteraceae

        Radish ->
            Brassicaceae

        Squash ->
            Cucurbitaceae

        Bean ->
            Fabaceae

        Peas ->
            Fabaceae

        Carot ->
            Apiaceae

        Scallion ->
            Alliaceae

        Garlic ->
            Alliaceae

        Shallot ->
            Alliaceae


category crop =
    case crop of
        Tomato ->
            Fruit

        Beetroot ->
            Root

        Salad ->
            Leaf

        Radish ->
            Root

        Squash ->
            Fruit

        Bean ->
            Seed

        Peas ->
            Seed

        Carot ->
            Root

        Scallion ->
            Leaf

        Garlic ->
            Bulb

        Shallot ->
            Bulb


allCrops =
    [ Tomato
    , Beetroot
    , Salad
    , Radish
    , Squash
    , Bean
    , Peas
    , Carot
    , Scallion
    , Garlic
    , Shallot
    ]


displayCropName crop =
    case crop of
        Tomato ->
            "Tomate"

        Beetroot ->
            "Betterave"

        Salad ->
            "Salade"

        Radish ->
            "Radis"

        Squash ->
            "Courge"

        Bean ->
            "Haricot"

        Peas ->
            "Pois"

        Carot ->
            "Carotte"

        Scallion ->
            "Ciboule"

        Garlic ->
            "Ail"

        Shallot ->
            "Échalotte"


displayCategoryName category =
    case category of
        Leaf ->
            "Feuille"

        Fruit ->
            "Fruit"

        Root ->
            "Racine"

        Seed ->
            "Graine"

        Bulb ->
            "Bulbe"
