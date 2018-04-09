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


gammas =
    [ F (ForAll "x" fG), T (Exists "x" fD) ]


deltas =
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


help =
    div [ class "rulesHelp" ]
        [ h2 [] [ text "Help" ]
        , Markdown.toHtml [] """
Use `&`, `/\\` or `∧` for conjunction, `|`, `\\/` or `∨` for disjunction, `->` or `→` for implication,
and `-`, `~` or `¬` for negation. Conjunction and disjunction are strictly binary. Each node of
the tableau contains a signed formula, i.e. it must be prefixed by `T` or `F`.

To write first order logic terms use '∀', '\\A', '\\forall', '\\a' and '∃', '\\E', '\\exists', '\\e' quantifiers.

"""
        , p []
            [ text "To enter a premise / assumption (which you want to prove), make it reference itself"
            , text """ (i.e. "(1) F ... [1]")."""
            ]
        , h3 [] [ text "α-rules" ]
        , div [] (List.map renderAlpha alphas)
        , h3 [] [ text "β-rules" ]
        , div [] (List.map renderBeta betas)
        , h3 [] [ text "γ-rules" ]
        , div [] (List.map renderDelta deltas)
        , h3 [] [ text "δ-rules" ]
        , div [] (List.map renderGamma gammas)
        ]



{- vim: set sw=2 ts=2 sts=2 et : -}
