module LogicContext exposing (..)

import Formula exposing (Formula, toString)
import Formula.Parser exposing (parse)
import List exposing (map)
import Parser exposing (deadEndsToString)
import Result.Extra exposing (combine)
import Set exposing (Set)


type alias ContextData =
    { axioms : List String
    , proovedTheorems : List String
    , newTheorem : String
    }


type alias LogicContext =
    { newTheorem : String
    , axiomsLookup : Set String
    , proovedTheoremsLookup : Set String
    }


type ValidationResult
    = IsAxiom
    | IsNewTheorem
    | IsProovedTheorem
    | Unknown


processFormula : String -> Result String String
processFormula formula =
    case parse formula of
        Ok f ->
            Ok <| toString f

        Err e ->
            Err ("Formula parsing failed (Invalid formula: " ++ deadEndsToString e ++ ")")


processFormulas : List String -> Result String (Set String)
processFormulas axioms =
    case combine (map parse axioms) of
        Ok formulas ->
            Ok <| Set.fromList (map toString formulas)

        Err e ->
            Err <| "Formula parsing failed (Invalid formula: " ++ deadEndsToString e ++ ")"


createContext : ContextData -> Result String LogicContext
createContext d =
    Result.map3
        (\newTheorem ->
            \axiomsLookup ->
                \theoremsLookup ->
                    LogicContext newTheorem axiomsLookup theoremsLookup
        )
        (processFormula d.newTheorem)
        (processFormulas d.axioms)
        (processFormulas d.proovedTheorems)


contextFindFormula : LogicContext -> Formula -> ValidationResult
contextFindFormula ctx f =
    let
        isAxiom =
            Set.member (toString f) ctx.axiomsLookup

        isNewTheorem =
            ctx.newTheorem == toString f

        isProovedTheorem =
            Set.member (toString f) ctx.proovedTheoremsLookup
    in
    if isAxiom then
        IsAxiom

    else if isNewTheorem then
        IsNewTheorem

    else if isProovedTheorem then
        IsProovedTheorem

    else
        Unknown
