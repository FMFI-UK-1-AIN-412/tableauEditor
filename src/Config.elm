module Config exposing
    ( Config
    , basicFol
    , basicPropositional
    , default
    , fullFol
    , fullPropositional
    )

import Dict exposing (Dict)


type alias Config =
    Dict String Bool


basicPropositionalRules : List String
basicPropositionalRules =
    [ "α", "β" ]


extendedPropositionalRules : List String
extendedPropositionalRules =
    [ "MP", "MT", "DS", "NCS", "ESTT", "ESTF", "ESFT", "ESFF" ]


nonAnalyticPropositionalRules : List String
nonAnalyticPropositionalRules =
    [ "Cut", "HS", "ECDT", "ECDF" ]


equalityRules : List String
equalityRules =
    [ "Reflexivity", "Leibnitz" ]


basicQuantifierRules : List String
basicQuantifierRules =
    [ "γ", "δ" ]


extendedQuantifierRules : List String
extendedQuantifierRules =
    [ "γ*", "δ*" ]


configFromRules : List String -> Config
configFromRules =
    Dict.fromList << List.map (\r -> ( r, True ))


default : Config
default =
    fullFol


basicPropositional : Config
basicPropositional =
    configFromRules basicPropositionalRules


fullPropositional : Config
fullPropositional =
    configFromRules <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules


basicFol : Config
basicFol =
    configFromRules <|
        basicPropositionalRules
            ++ basicQuantifierRules


fullFol : Config
fullFol =
    configFromRules <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules
            ++ equalityRules
            ++ basicQuantifierRules
            ++ extendedQuantifierRules
