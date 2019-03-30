module Formula exposing (Formula(..), Signed(..), SignedType(..), binary, errorString, form, formula, isAlpha, isBeta, isSignedComplementary, isSignedSubformulaOf, isSubformulaOf, negSigned, negType, oneOfSymbols, parse, parseSigned, sform, signedFormula, signedGetFormula, signedSubformulas, signedType, strFormula, strSigned, subformulas)

import Char
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , end
        , float
        , keyword
        , lazy
        , oneOf
        , succeed
        , symbol
        , sequence
        , variable
        , spaces
        , map
        , andThen
        )
import Set
import Debug exposing (toString)


type Formula
    = Atom String
    | Neg Formula
    | Disj Formula Formula
    | Conj Formula Formula
    | Impl Formula Formula
    | FF
    | FT


subformulas : Formula -> List Formula
subformulas f =
    case f of
        Neg sf ->
            [ sf ]

        Disj lf rf ->
            [ lf, rf ]

        Conj lf rf ->
            [ lf, rf ]

        Impl lf rf ->
            [ lf, rf ]

        _ ->
            []


isSubformulaOf : Formula -> Formula -> Bool
isSubformulaOf a b =
    List.member a (subformulas b)



--
-- Signed formulas
--


type Signed a
    = T a
    | F a


type SignedType
    = Alpha
    | Beta


negType : SignedType -> SignedType
negType t =
    case t of
        Alpha ->
            Beta

        Beta ->
            Alpha


negSigned : Signed Formula -> Signed Formula
negSigned sf =
    case sf of
        T f ->
            F f

        F f ->
            T f


signedType : Signed Formula -> SignedType
signedType sf =
    case sf of
        T FF ->
            Alpha

        T FT ->
            Alpha

        T (Atom _) ->
            Alpha

        F (Atom _) ->
            Alpha

        T (Neg _) ->
            Alpha

        F (Neg _) ->
            Alpha

        T (Conj _ _) ->
            Alpha

        T (Disj _ _) ->
            Beta

        T (Impl _ _) ->
            Beta

        F f ->
            negType <| signedType <| T f


isAlpha : Signed Formula -> Bool
isAlpha x =
    Alpha == signedType x


isBeta : Signed Formula -> Bool
isBeta x =
    Beta == signedType x


signedSubformulas : Signed Formula -> List (Signed Formula)
signedSubformulas sf =
    case sf of
        T (Neg f) ->
            [ F f ]

        T (Conj l r) ->
            [ T l, T r ]

        T (Disj l r) ->
            [ T l, T r ]

        T (Impl l r) ->
            [ F l, T r ]

        T _ ->
            []

        F f ->
            T f |> signedSubformulas |> List.map negSigned


isSignedSubformulaOf : Signed Formula -> Signed Formula -> Bool
isSignedSubformulaOf a b =
    List.member a (signedSubformulas b)


isSignedComplementary : Signed Formula -> Signed Formula -> Bool
isSignedComplementary a b =
    case ( a, b ) of
        ( T x, F y ) ->
            x == y

        ( F x, T y ) ->
            x == y

        _ ->
            False


signedGetFormula : Signed Formula -> Formula
signedGetFormula sf =
    case sf of
        T f ->
            f

        F f ->
            f



--
-- Parsing
--


parseSigned =
    Parser.run (succeed identity |. spaces |= signedFormula |. spaces |. end)


signedFormula =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed T
                |. keyword "T"
                |. spaces
                |= formula
            , succeed F
                |. keyword "F"
                |. spaces
                |= formula
            ]



-- parse = Parser.run formula


parse =
    Parser.run (succeed identity |. spaces |= formula |. spaces |. end)


errorString : List Parser.DeadEnd -> String
errorString e =
    "Invalid formula: " ++ toString e


formula : Parser Formula
formula =
    oneOf
        [ succeed Atom
            |= variable
                { start = Char.isLower
                , inner = Char.isLower
                , reserved = Set.fromList []
                }
        , succeed Neg
            |. oneOfSymbols [ "-", "¬", "~" ]
            |. spaces
            |= lazy (\_ -> formula)
        , lazy (\_ -> binary [ "&", "∧", "/\\" ] Conj)
        , lazy (\_ -> binary [ "|", "∨", "\\/" ] Disj)
        , lazy (\_ -> binary [ "->", "→" ] Impl)
        , succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> formula)
            |. spaces
            |. symbol ")"
        ]


binary conn constructor =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> formula)
        |. spaces
    |> andThen (\lhs ->
        map (constructor lhs) <|
            succeed identity
                |. oneOfSymbols conn
                |. spaces
                |= lazy (\_ -> formula)
                |. spaces
                |. symbol ")"
    )


oneOfSymbols syms =
    oneOf (List.map symbol syms)


strSigned sf =
    case sf of
        T f ->
            "T " ++ strFormula f

        F f ->
            "F " ++ strFormula f


strFormula f =
    case f of
        FT ->
            "True"

        FF ->
            "False"

        Atom a ->
            a

        Neg nf ->
            "¬" ++ strFormula nf

        Conj lf rf ->
            "(" ++ strFormula lf ++ " ∧ " ++ strFormula rf ++ ")"

        Disj lf rf ->
            "(" ++ strFormula lf ++ " ∨ " ++ strFormula rf ++ ")"

        Impl lf rf ->
            "(" ++ strFormula lf ++ " → " ++ strFormula rf ++ ")"



--
-- helper funcs
--


sform : String -> Signed Formula
sform =
    parseSigned >> Result.withDefault (T FF)


form : String -> Formula
form =
    parse >> Result.withDefault FF



{- vim: set sw=2 ts=2 sts=2 et : -}
