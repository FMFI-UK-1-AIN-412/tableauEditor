module Config exposing
    ( Config(..)
    , default
    , getRuleSet
    , toString
    )

import Set exposing (Set)


type Config
    = BasicPropositional
    | Propositional
    | PropositionalWithEquality
    | BasicFol
    | FullFol


type alias RuleSet =
    Set String


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


default : Config
default =
    BasicPropositional


toString : Config -> String
toString config =
    case config of
        BasicPropositional ->
            "Basic propositional"

        Propositional ->
            "Propositional"

        PropositionalWithEquality ->
            "Propositional with equality"

        BasicFol ->
            "Basic FOL"

        FullFol ->
            "Full FOL"


getRuleSet : Config -> RuleSet
getRuleSet config =
    case config of
        BasicPropositional ->
            basicPropositionalRuleSet

        Propositional ->
            propositionalRuleSet

        PropositionalWithEquality ->
            propositionalWithEqualityRuleSet

        BasicFol ->
            basicFolRuleSet

        FullFol ->
            fullFolRuleSet


basicPropositionalRuleSet : RuleSet
basicPropositionalRuleSet =
    Set.fromList <|
        basicPropositionalRules


propositionalRuleSet : RuleSet
propositionalRuleSet =
    Set.fromList <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules


propositionalWithEqualityRuleSet : RuleSet
propositionalWithEqualityRuleSet =
    Set.fromList <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules
            ++ equalityRules


basicFolRuleSet : RuleSet
basicFolRuleSet =
    Set.fromList <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules
            ++ equalityRules
            ++ basicQuantifierRules


fullFolRuleSet : RuleSet
fullFolRuleSet =
    Set.fromList <|
        basicPropositionalRules
            ++ extendedPropositionalRules
            ++ nonAnalyticPropositionalRules
            ++ equalityRules
            ++ basicQuantifierRules
            ++ extendedQuantifierRules
