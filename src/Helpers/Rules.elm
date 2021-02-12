module Helpers.Rules exposing (..)

import Dict
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Html exposing (Attribute, Html, div, h2, h3, p, table, td, text, tr)
import Html.Attributes exposing (..)
import Markdown
import Term exposing (Term(..))


fA =
    PredAtom "A" []


fB =
    PredAtom "B" []


alphas =
    [ T (Conj fA fB)
    , F (Disj fA fB)
    , F (Impl fA fB)
    , T (Neg fA)
    , F (Neg fA)
    ]


betas =
    [ F (Conj fA fB)
    , T (Disj fA fB)
    , T (Impl fA fB)
    ]


fG =
    PredAtom "P" [ Var "x" ]


fD =
    PredAtom "P" [ Var "x" ]


deltas =
    [ F (ForAll "x" fG), T (Exists "x" fD) ]


gammas =
    [ T (ForAll "x" fD), F (Exists "x" fD) ]


renderAlpha : Signed Formula -> Html msg
renderAlpha a =
    table [ class "rule" ] <|
        tr [] [ td [] [ text <| Formula.Signed.toString a ] ]
            :: List.map
                (\f -> tr [] [ td [] [ text <| Formula.Signed.toString f ] ])
                (Formula.Signed.subformulas a)


renderBeta b =
    let
        subfs =
            Formula.Signed.subformulas b
    in
    table [ class "rule" ]
        [ tr [] [ td [ colspan (List.length subfs) ] [ text <| Formula.Signed.toString b ] ]
        , tr [] <|
            List.map
                (\f -> td [] [ text <| Formula.Signed.toString f ])
                subfs
        ]


renderGamma g =
    table [ class "rule" ] <|
        tr [] [ td [] [ text <| Formula.Signed.toString g ] ]
            :: List.map
                (\f ->
                    tr []
                        [ td []
                            [ text <| Formula.Signed.toString <| demoSubst "x" "t" f ]
                        ]
                )
                (Formula.Signed.subformulas g)


renderDelta d =
    table [ class "rule" ] <|
        tr [] [ td [] [ text <| Formula.Signed.toString d ] ]
            :: List.map
                (\f ->
                    tr []
                        [ td []
                            [ text <| Formula.Signed.toString <| demoSubst "x" "y" f ]
                        ]
                )
                (Formula.Signed.subformulas d)


demoSubst : String -> String -> Signed Formula -> Signed Formula
demoSubst x y =
    signedMap
        (Result.withDefault FF
            << Formula.substitute (Dict.singleton x (Var y))
        )


signedMap : (a -> a) -> Signed a -> Signed a
signedMap f sx =
    case sx of
        T x ->
            T (f x)

        F x ->
            F (f x)


symbolsTable =
    div [ class "half" ]
        [ h3 [] [ text "Propositional and first-order logical symbols" ]
        , Html.table [ class "rulesHelpTable" ]
            [ Html.tr []
                [ Html.th [] [ text "Negation" ]
                , Html.th [] [ text "Conjunction" ]
                , Html.th [] [ text "Disjunction" ]
                , Html.th [] [ text "Implication" ]
                , Html.th [] [ text "Universal quantifier" ]
                , Html.th [] [ text "Existential quantifier" ]
                ]
            , Html.tr []
                [ Html.td [] [ Markdown.toHtml [ class "symbols" ] "`-`, `~`, `¬`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`&`, `/\\`, `∧`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`|`, `\\/`, `∨`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`->`, `→`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`∀`, `\\A`, `\\forall`, `\\a`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`∃`, `\\E`, `\\exists`, `\\e`" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "unary" ]
                , Html.td [ colspan 3 ]
                    [ text "strictly binary, must be parenthesized" ]
                , Html.td [ colspan 2 ]
                    [ text "takes a\u{00A0}variable and a formula" ]
                ]
            ]
        ]


notesTable =
    div [ class "half" ]
        [ Html.h3 [] [ text "Important notes" ]
        , Html.table [ class "rulesHelpTable" ]
            [ Html.tr []
                [ Html.th [] [ text "Note" ]
                , Html.th [] [ text "Example" ]
                ]
            , Html.tr []
                [ Html.td []
                    [ Markdown.toHtml [ class "symbols" ]
                        "Each node contains a signed formula, i.e. it must be prefixed by `T` or `F`. "
                    ]
                , Html.td []
                    [ Markdown.toHtml [ class "symbols" ]
                        "**T** \\forall x P(x)"
                    , Markdown.toHtml [ class "symbols" ]
                        "**F** ∃x ∀y (K(x,q) ∧ G(y,x))"
                    ]
                ]
            , Html.tr []
                [ Html.td []
                    [ Markdown.toHtml [ class "symbols" ]
                        "Write each premise/assumption and conclusion/goal with no references. Sign premises with `T` and sign conclusions with\u{00A0}`F`."
                    ]
                , Html.td []
                    [ Markdown.toHtml [ class "symbols" ]
                        "(**1**) **T** (A → B) [**1**]"
                    , Markdown.toHtml [ class "symbols" ]
                        "(**2**) **F** ¬(A ∧ ¬B) [**2**]"
                    ]
                ]
            , Html.tr []
                [ Html.td []
                    [ Markdown.toHtml [ class "symbols" ]
                        "When substituting a\u{00A0}variable\u{00A0}_x_ with a term\u{00A0}_t_, _t_\u{00A0}must not contain any variable which is bound at any occurrence of\u{00A0}_x_."
                    ]
                , Html.td []
                    [ Markdown.toHtml [ class "symbols" ] "**Incorrect** example: "
                    , Markdown.toHtml [ class "symbols" ] "(1) T ∀x ∃**y** P(**x**,y) [1]"
                    , Markdown.toHtml [ class "symbols" ] "(2) T ∃y P(f(y),y) {x→**f(y)**}\u{00A0}[1]"
                    ]
                ]
            , Html.tr []
                [ Html.td [] [ Markdown.toHtml [ class "symbols" ] "When applying a\u{00A0}δ\u{00A0}rule, substitute the bound variable with **a new** constant/variable, i.e., one which is not free (or, even better, does not occur at all) in any node above the current one." ]
                , Html.td []
                    [ Markdown.toHtml [ class "symbols" ] "**Incorrect** example: "
                    , Markdown.toHtml [ class "symbols" ] "(1) T L(**p**) [1]"
                    , Markdown.toHtml [ class "symbols" ] "(2) T ∃x ∀y P(x,y) [2]"
                    , Markdown.toHtml [ class "symbols" ] "(3) T ∀y P(**p**,y) {x→**p**} [2]"
                    ]
                ]
            ]
        ]


rulesTable =
    div [ class "full" ]
        [ h3 [] [ text "Applying rules" ]
        , Html.table [ class "rulesHelpTable" ]
            [ Html.tr []
                [ Html.th [] [ text "" ]
                , Html.th [] [ text "α-rule" ]
                , Html.th [] [ text "β-rule" ]
                , Html.th [] [ text "γ-rule" ]
                , Html.th [] [ text "δ-rule" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "rules" ]
                , Html.td [] [ div [] (List.map renderAlpha alphas) ]
                , Html.td [] [ div [] (List.map renderBeta betas) ]
                , Html.td [] [ div [] (List.map renderGamma gammas) ]
                , Html.td [] [ div [] (List.map renderDelta deltas) ]
                ]
            , Html.tr []
                [ Html.td [] [ text "example" ]
                , Html.td []
                    [ div [ class "formula" ]
                        [ text "(1) T(a∧b) [1]"
                        , div [ class "alpha" ]
                            [ div [ class "formula" ]
                                [ text "(2) T a [1]"
                                , div [ class "alpha" ] [ div [ class "formula" ] [ text "(3) T b [1]" ] ]
                                ]
                            ]
                        ]
                    ]
                , Html.td []
                    [ div [ class "formula" ]
                        [ text "(1) T(a∨b) [1]"
                        , div [ class "beta" ]
                            [ div [ class "formula" ] [ text "(2) T a [1]" ]
                            , div [ class "formula" ] [ text "(3) T b [1]" ]
                            ]
                        ]
                    ]
                , Html.td []
                    [ div [ class "formula" ]
                        [ text "(1) T ∀x P(x) [1]"
                        , div [ class "gamma" ]
                            [ div [ class "formula" ]
                                [ text "(2) T P(g(k,y)) {x→g(k,y)} [1]" ]
                            ]
                        ]
                    ]
                , Html.td []
                    [ div [ class "formula" ]
                        [ text "(1) F ∀x P(x) [1]"
                        , div [ class "delta" ]
                            [ div [ class "formula" ]
                                [ text "(2) F P(z) {x→z} [1]" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


help =
    div [ class "rulesHelp" ]
        [ h2 [] [ text "Help" ]
        , symbolsTable
        , notesTable
        , rulesTable
        ]



{- vim: set sw=2 ts=2 sts=2 et : -}
