module Help exposing (..)
import Html exposing (Html, Attribute, div, table, tr, td, text, p, h2, h3)
import Html.Attributes exposing (..)
import Formula exposing (..)
import Markdown


fA = Atom "A"
fB = Atom "B"

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

renderAlpha : (Signed Formula) -> Html msg
renderAlpha a =
  div [ class "rule" ]
  [ table [ class "tableau" ]
    (
      [ tr []
        [ td [ class "beta" ]
          [ text <| strSigned a ]
        ]
      ] ++ List.map (\f ->
          tr [] [ td [] [ text <| strSigned f ] ]
        ) (signedSubformulas a)
    )
  ]

renderBeta b =
  div [ class "rule" ]
  [ table [ class "tableau" ]
    [ tr [] [ td [ class "beta", colspan 2 ] [ text <| strSigned b ] ]
    , tr []
      (List.map (\f ->
        td [] [ text <| strSigned f ]
      ) (signedSubformulas b))
    ]
  ]

help =
  div [ class "rulesHelp" ]
  [ h2 [] [ text "Help" ]
  , Markdown.toHtml [] """
Use `&`, `/\\`, or `∧` for conjunction, `|`, `\\/`, or `∨` for disjunction,
`->` or `→` for implication, `-`, `~` or `¬` for negation.
Conjunction and disjunction are strictly binary.

Each node of the tableau contains a signed formula, i.e., it must be
prefixed by `T` or `F`.
"""
  , p []
    [ text "To enter a premise / assumption (which you want to prove), make it reference itself"
    , text """ (i.e. "(1) F ... [1]")."""
    ]
  , h3 [] [ text "α-rules" ]
  , div [] (List.map renderAlpha alphas)
  , h3 [] [ text "β-rules" ]
  , div [] (List.map renderBeta betas)
  ]

{- vim: set sw=2 ts=2 sts=2 et : -}
