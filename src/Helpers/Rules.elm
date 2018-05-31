module Rules exposing (..)

import Formula exposing (..)
import Html exposing (Attribute, Html, div, h2, h3, p, table, td, text, tr)
import Html.Attributes exposing (..)
import Markdown


fA =
    Atom "A" []


fB =
    Atom "B" []


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
    Atom "P" [ Var "x" ]


fD =
    Atom "P" [ Var "x" ]


deltas =
    [ F (ForAll "x" fG), T (Exists "x" fD) ]


gammas =
    [ T (ForAll "x" fD), F (Exists "x" fD) ]


renderAlpha : Signed Formula -> Html msg
renderAlpha a =
    div [ class "rule" ]
        [ table [ class "tableau" ]
            ([ tr []
                [ td [ class "beta" ]
                    [ text <| strSigned a ]
                ]
             ]
                ++ List.map
                    (\f ->
                        tr [] [ td [] [ text <| strSigned f ] ]
                    )
                    (signedSubformulas a)
            )
        ]


renderBeta b =
    div [ class "rule" ]
        [ table [ class "tableau" ]
            [ tr [] [ td [ class "beta", colspan 2 ] [ text <| strSigned b ] ]
            , tr []
                (List.map
                    (\f ->
                        td [] [ text <| strSigned f ]
                    )
                    (signedSubformulas b)
                )
            ]
        ]


renderGamma g =
    div [ class "rule" ]
        [ table [ class "tableau" ]
            ([ tr []
                [ td [ class "alpha" ]
                    [ text <| strSigned g ]
                ]
             ]
                ++ List.map
                    (\f ->
                        tr [] [ td [] [ text <| strSigned f ] ]
                    )
                    (signedSubformulas g)
            )
        ]


renderDelta d =
    div [ class "rule" ]
        [ table [ class "tableau" ]
            ([ tr []
                [ td [ class "alpha" ]
                    [ text <| strSigned d ]
                ]
             ]
                ++ List.map
                    (\f ->
                        tr [] [ td [] [ text <| strSigned f ] ]
                    )
                    (signedSubformulas d)
            )
        ]


symbolsTable =
    div [ class "half" ]
        [ h3 [] [ text "Symbols of propositional and first-order logic" ]
        , Html.table [ class "rulesHelpTable" ]
            [ Html.tr []
                [ Html.th [] [ text "Symbols of conjunction" ]
                , Html.th [] [ text "Symbols of disjunction" ]
                , Html.th [] [ text "Symbols of implication" ]
                , Html.th [] [ text "Symbols of negation" ]
                , Html.th [] [ text "Universal quantifier" ]
                , Html.th [] [ text "Existential quantifier" ]
                ]
            , Html.tr []
                [ Html.td [] [ Markdown.toHtml [ class "symbols" ] "`&`, `/\\`, `∧`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`|`, `\\/`, `∨`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`->`, `→`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`-`, `~`, `¬`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`∀`, `\\A`, `\\forall`, `\\a`" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`∃`, `\\E`, `\\exists`, `\\e`" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "strictly binary" ]
                , Html.td [] [ text "strictly binary" ]
                , Html.td [] [ text "strictly binary" ]
                , Html.td [] [ text "unary" ]
                , Html.td [] [ text "First order logic term" ]
                , Html.td [] [ text "First order logic term" ]
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
                [ Html.td [] [ Markdown.toHtml [ class "symbols" ] "Each of the nodes contains a signed formula, i.e. it must be prefixed by `T` or `F`. " ]
                , Html.td []
                    [ Markdown.toHtml [ class "symbols" ] "T \\forall x P(x)"
                    , Markdown.toHtml [ class "symbols" ] "F \\exists x \\forall p (K(x, q) ∧ G(p, x) )"
                    ]
                ]
            , Html.tr []
                [ Html.td [] [ Markdown.toHtml [ class "symbols" ] "To enter a premise / assumption (which you want to prove), make it reference itself" ]
                , Html.td []
                    [ Markdown.toHtml [ class "symbols" ] "(1) T (a → b) [1]"
                    , Markdown.toHtml [ class "symbols" ] """(i.e. "(1) F .................. [1]")"""
                    ]
                ]
            , Html.tr []
                [ Html.td [] [ Markdown.toHtml [ class "symbols" ] "When substituting, choose only such term which does not contain a variable which looks like bound in referrenced formula." ]
                , Html.td []
                    [ Markdown.toHtml [ class "symbols" ] "wrong example: "
                    , Markdown.toHtml [ class "symbols" ] "(1) T \\forall x \\exists k P(x,k) [1]"
                    , Markdown.toHtml [ class "symbols" ] "(2) T \\exists k P(k,k) {x→k}[1]"
                    ]
                ]
            , Html.tr []
                [ Html.td [] [ Markdown.toHtml [ class "symbols" ] "When applying delta rule make sure to use completely new constant, which was not used as free (better bound as well) in a node somewhere above." ]
                , Html.td []
                    [ Markdown.toHtml [ class "symbols" ] "wrong example: "
                    , Markdown.toHtml [ class "symbols" ] "(1) T L(p) [1]"
                    , Markdown.toHtml [ class "symbols" ] "(2) T \\exists x \\forall k P(x,k) [2]"
                    , Markdown.toHtml [ class "symbols" ] "(3) T \\forall k P(p,k) {x→p} [2]"
                    ]
                ]
            ]
        ]


rulesTable =
    div []
        [ h3 [] [ text "Aplying rules" ]
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
                        [ text "(1) T \\forall x P(x) [1]"
                        , div [ class "gamma" ]
                            [ div [ class "formula" ]
                                [ text "(2) T P(k) {x→k} [1]" ]
                            ]
                        ]
                    ]
                , Html.td []
                    [ div [ class "formula" ]
                        [ text "(1) T \\forall x P(x) [1]"
                        , div [ class "delta" ]
                            [ div [ class "formula" ]
                                [ text "(2) T P(k) {x→k} [1]" ]
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
