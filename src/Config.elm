module Config exposing
    ( Config
    , basicFol
    , basicPropositional
    , default
    , fullFol
    , propositional
    , propositionalWithEquality
    , toString
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


toString : Config -> String
toString config =
    if config == basicPropositional then
        "Basic propositional"

    else if config == propositional then
        "Propositional"

    else if config == propositionalWithEquality then
        "Propositional with equality"

    else if config == basicFol then
        "Basic FOL"

    else if config == fullFol then
        "Full FOL"

    else
        "unknown configuration"


basicPropositional : Config
basicPropositional =
    configFromRules basicPropositionalRules


propositional : Config
propositional =
    configFromRules <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules


propositionalWithEquality : Config
propositionalWithEquality =
    configFromRules <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules
            ++ equalityRules


basicFol : Config
basicFol =
    configFromRules <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules
            ++ equalityRules
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
