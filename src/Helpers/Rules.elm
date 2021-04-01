module Helpers.Rules exposing (..)

import Dict
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Html exposing (Attribute, Html, code, div, h2, h3, p, span, sub, sup, table, td, text, tr)
import Html.Attributes exposing (..)
import Markdown
import Term exposing (Term(..))
import Tableau exposing (UnaryWithSubstExtType(..))


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


reflexivityFormulas =
    [ ( "", "", "T t≐t" ) ]


mpFormulas =
    [ ( "T (A→B)", "T A", "T B" ) ]


mtFormulas =
    [ ( "T (A→B)", "F B", "F A" ) ]


dsFormulas =
    [ ( "T (A∨B)", "F A", "T B" ), ( "T (A∨B)", "F B", "T A" ) ]


ncsFormulas =
    [ ( "F (A∧B)", "T A", "F B" ), ( "F (A∧B)", "T B", "F A" ) ]


cutFormulas =
    [ ( "", "T A", "F A" ) ]


hsFormulas =
    [ ( "T (A→B)", "T (B→C)", "T (A→C)" ) ]


esttFormulas =
    [ ( "T (A↔B)", "T A", "T B" ), ( "T (A↔B)", "T B", "T A" ) ]


estfFormulas =
    [ ( "T (A↔B)", "F A", "F B" ), ( "T (A↔B)", "F B", "F A" ) ]


esftFormulas =
    [ ( "F (A↔B)", "T A", "F B" ), ( "F (A↔B)", "T B", "F A" ) ]


esffFormulas =
    [ ( "F (A↔B)", "F A", "T B" ), ( "F (A↔B)", "F B", "T A" ) ]


ecdtFormulas =
    [ ( "T (A↔B)", "T (A∧B)", "F (A∨B)" ) ]


ecdfFormulas =
    [ ( "F (A↔B)", "T (A∧¬B)", "F (A∨¬B)" ) ]


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


renderUnary : ( String, String, String ) -> Html msg
renderUnary ( f1, f2, f3 ) =
    table [ class "rule", class "withWhiteSpace" ] <|
        [ tr [] [ td [] [ text <| (f1 ++ "   " ++ f2) ] ]
        , tr [] [ td [] [ text <| f3 ] ]
        ]


renderBinary : ( String, String, String ) -> Html msg
renderBinary ( f, sf1, sf2 ) =
    table [ class "rule" ]
        [ div [ class "formula" ]
            [ text <| f
            , div [ class "beta" ]
                [ div [ class "formula" ] [ text <| sf1 ]
                , div [ class "formula" ] [ text <| sf2 ]
                ]
            ]
        ]


renderGammaDeltaStar ( sign, quant ) =
    table [ class "rule", class "withWhiteSpace" ] <|
        [ tr [] [ td [] [ text (sign ++ " " ++ quant ++ "x"), subs "1", text (",...," ++ quant ++ "x"), subs "n", text "A" ] ]
        , tr [] [ td [] [ text (sign ++ " A{x"), subs "1", text "→y", subs "1", text ",...,x", subs "n", text "→y", subs "n", text "}" ] ]
        ]


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
                [ Html.th [] [ text "Logical symbol" ]
                , Html.th [] [ text "Symbols" ]
                , Html.th [] [ text "Restrictions" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Negation" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`-`, `~`, `¬`" ]
                , Html.td [] [ text "unary" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Equality" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`=`, `≐`" ]
                , Html.td [ rowspan 2 ] [ text "binary, takes two terms" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Inequality" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`!=`, `/=`, `≠`" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Conjunction" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`&`, `/\\`, `∧`" ]
                , Html.td [ rowspan 4 ]
                    [ text "strictly binary, must be parentdesized" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Disjunction" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`|`, `\\/`, `∨`" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Implication" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`->`, `→`" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Equivalence" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`<->`, `↔`" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Universal quantifier" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`∀`, `\\A`, `\\forall`, `\\a`" ]
                , Html.td [ rowspan 2 ]
                    [ text "takes a\u{00A0}variable and a formula" ]
                ]
            , Html.tr []
                [ Html.td [] [ text "Existential quantifier" ]
                , Html.td [] [ Markdown.toHtml [ class "symbols" ] "`∃`, `\\E`, `\\exists`, `\\e`" ]
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


linearExample a b c =
    Html.td []
        [ div [ class "formula" ]
            [ text a
            , div [ class "alpha" ]
                [ div [ class "formula" ]
                    [ text b
                    , div [ class "alpha" ] [ div [ class "formula" ] [ text c ] ]
                    ]
                ]
            ]
        ]


binaryExample a b c =
    Html.td []
        [ div [ class "formula" ]
            [ text a
            , div [ class "beta" ]
                [ div [ class "formula" ] [ text b ]
                , div [ class "formula" ] [ text c ]
                ]
            ]
        ]


subs txt =
    sub [] [ text txt ]


sups txt =
    sup [] [ text txt ]


ruleItem : String -> String -> List (Html msg) -> Html msg -> List (Html msg)
ruleItem ruleName fullName formulas example = 
    [ Html.tr []
        [ Html.th [] [ text <| ruleName ++ " rule"  ++ fullName] 
        , Html.th [] [ text "example" ]
        ]
    , Html.tr []
        [ Html.td [] [ div[] formulas]
        , example
        ]
    ]

alphaItem : List (Html msg)
alphaItem = 
    ruleItem 
        "α" 
        ""
        (List.map renderAlpha alphas)
        (linearExample "(1) T(a∧b) [ ]" "(2) T a [1]" "(3) T b [1]")


betaItem : List (Html msg)
betaItem = 
    ruleItem 
        "β" 
        ""
        (List.map renderBeta betas)
        (binaryExample "(1) T(a∨b) [ ]" "(2) T a [1]" "(3) T b [1]")


gammaItem : List (Html msg)
gammaItem = 
    ruleItem 
        "γ" 
        ""
        (List.map renderGamma gammas)
        (linearExample "" "(1) T ∀x P(x) [ ]" "(2) T P(g(k,y)) {x→g(k,y)} [1]")


deltaItem : List (Html msg)
deltaItem = 
    ruleItem 
        "δ" 
        ""
        (List.map renderDelta deltas) 
        (linearExample "" "(1) F ∀x P(x) [ ]" "(2) F P(z) {x→z} [1]")


gammaStarItem : List (Html msg)
gammaStarItem = 
    ruleItem 
        "γ*" 
        ""
        (List.map renderGammaDeltaStar [ ( "T", "∀" ), ( "F", "∃" ) ])
        (linearExample "" "(1) T ∀x∀y P(x,y) [ ]" "(2) T P(g(k,z), q) {x→g(k,z) ,y→q} [1]")


deltaStarItem : List (Html msg)
deltaStarItem = 
    ruleItem 
        "γ*" 
        ""
        (List.map renderGammaDeltaStar [ ( "F", "∀" ), ( "T", "∃" ) ])
        (linearExample "" "(1) F ∀x∀y P(x,y) [ ]" "(2) F P(q,z) {x→q, y→z} [1]")


reflexivityItem : List (Html msg)
reflexivityItem = 
    ruleItem 
        "Reflexivity" 
        ""
        (List.map renderUnary reflexivityFormulas)
        (linearExample "(1) T a≐a [ ]" "" "")


leibnitzItem : List (Html msg)
leibnitzItem = 
    ruleItem
        "Leibnitz"
        ""
        ( [ table [ class "rule", class "withWhiteSpace" ] <|
                [ tr [] [ td [] [ text "T t", subs "1", text "≐t", subs "2", text "  A", sups "+", text "{q→t", subs "1", text "}" ] ]
                , tr [] [ td [] [ text "   A", sups "+", text "{q→t", subs "2", text "}" ] ]
                ]
            ])
        (linearExample "(1) T x≐f(y) [ ]" "(2) T p(x) [ ]" "(3) T p(f(y)) [1,2]")


cutItem : List (Html msg)
cutItem = 
    ruleItem 
        "Cut" 
        ""
        (List.map renderBinary cutFormulas)
        (binaryExample "" "T a [ ]" "F a [ ]")


modusPonensItem : List (Html msg)
modusPonensItem = 
    ruleItem 
        "MP" 
        " (Modus Ponens)"
        (List.map renderUnary mpFormulas)
        (linearExample "(1) T(a→b) [ ]" "(2) T a [ ]" "(3) T b [1,2]")


modusTolensItem : List (Html msg)
modusTolensItem = 
    ruleItem 
        "MT" 
        " (Modus Tolens)"
        (List.map renderUnary mtFormulas)
        (linearExample "(1) T(a→b) [ ]" "(2) F b [ ]" "(3) F a [1,2]")


hsItem : List (Html msg)
hsItem = 
    ruleItem 
        "HS" 
        ""
        (List.map renderUnary hsFormulas)
        (linearExample "(1) T(a→b) [ ]" "(2) T(b→c) [ ]" "(3) T(a→c) [1,2]")


dsItem : List (Html msg)
dsItem = 
    ruleItem 
        "DS" 
        ""
        (List.map renderUnary dsFormulas)
        (linearExample "(1) T(a∨b) [ ]" "(2) F a [ ]" "(3) T b [1,2]")


ncsItem : List (Html msg)
ncsItem = 
    ruleItem 
        "NCS" 
        ""
        (List.map renderUnary ncsFormulas)
        (linearExample "(1) F(a∧b) [ ]" "(2) T a [ ]" "(3) F b [1,2]")


esttItem : List (Html msg)
esttItem = 
    ruleItem 
        "ESTT" 
        ""
        (List.map renderUnary esttFormulas)
        (linearExample "(1) T(a↔b) [ ]" "(2) T b [ ]" "(3) T a [1,2]")


estfItem : List (Html msg)
estfItem = 
    ruleItem 
        "ESTF" 
        ""
        (List.map renderUnary estfFormulas)
        (linearExample "(1) T(a↔b) [ ]" "(2) F a [ ]" "(3) F b [1,2]")


esftItem : List (Html msg)
esftItem = 
    ruleItem 
        "ESFT" 
        ""
        (List.map renderUnary esftFormulas)
        (linearExample "(1) F(a↔b) [ ]" "(2) T a [ ]" "(3) F b [1,2]")


esffItem : List (Html msg)
esffItem = 
    ruleItem 
        "ESFF" 
        ""
        (List.map renderUnary esffFormulas)
        (linearExample "(1) F(a↔b) [ ]" "(2) F b [ ]" "(3) T a [1,2]")


ecdtItem : List (Html msg)
ecdtItem = 
    ruleItem 
        "ECDT" 
        ""
        (List.map renderBinary ecdtFormulas)
        (binaryExample "(1) T(a↔b) [ ]" "(2) T(a∧b) [1]" "(3) F (a∨b) [1]")


ecdfItem : List (Html msg)
ecdfItem = 
    ruleItem 
        "ECDF" 
        ""
        (List.map renderBinary ecdfFormulas)
        (binaryExample "(1) F(a↔b) [ ]" "(2) F T (a∧¬b) [1]" "(3) F T (a∨¬b) [1]")


firstRuleColumn =
    div [ class "half" ]
        [ Html.table [ class "rulesHelpTable", class "delete" ] <|
            alphaItem
            ++ betaItem
            ++ gammaItem
            ++ deltaItem
            ++ gammaStarItem
            ++ deltaStarItem
            ++ reflexivityItem
            ++ leibnitzItem
            ++ modusPonensItem
            ++ modusTolensItem
        ]


secondRuleColumn =
    div [ class "half" ]
        [ Html.table [ class "rulesHelpTable", class "delete" ] <|
            hsItem
            ++ dsItem
            ++ ncsItem
            ++ esffItem
            ++ esftItem
            ++ estfItem
            ++ esttItem
            ++ ecdtItem
            ++ ecdfItem
        ]


help =
    div [ class "rulesHelp" ]
        [ h2 [] [ text "Help" ]
        , symbolsTable
        , notesTable
        , h3 [class "full"] [ text "Applying rules" ]
        , firstRuleColumn
        , secondRuleColumn
        ]



{- vim: set sw=2 ts=2 sts=2 et : -}
