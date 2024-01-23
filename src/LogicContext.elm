module LogicContext exposing
    ( ContextData
    , LogicContext
    , FormulaCategory(..)
    , contextFormulaCategories
    , createContext
    )

import Formula exposing (Formula, toString)
import Formula.Parser exposing (parse)
import List exposing (filter, map)
import Parser exposing (deadEndsToString)
import Result.Extra exposing (combine)
import Set exposing (Set)


type alias ContextData =
    { axioms : List String
    , provedTheorems : List String
    , newTheorem : String
    }


type alias LogicContext =
    { newTheorem : String
    , axiomsLookup : Set String
    , provedTheoremsLookup : Set String
    }


type FormulaCategory
    = Axiom
    | NewTheorem
    | ProvedTheorem


processFormula : String -> String -> Result String String
processFormula kind formula =
    case parse formula of
        Ok f ->
            Ok <| toString f

        Err e ->
            Err ("Failed to parse " ++ kind ++ ": “" ++ formula ++ "”. Error: " ++ deadEndsToString e)


processFormulas : String -> List String -> Result String (Set String)
processFormulas kind axioms =
    case combine (map (processFormula kind) axioms) of
        Ok formulas ->
            Ok <| Set.fromList formulas

        Err e ->
            Err e


createContext : ContextData -> Result String LogicContext
createContext d =
    Result.map3
        (\newTheorem ->
            \axiomsLookup ->
                \theoremsLookup ->
                    LogicContext newTheorem axiomsLookup theoremsLookup
        )
        (processFormula "theorem to be proved" d.newTheorem)
        (processFormulas "axiom" d.axioms)
        (processFormulas "theorem" d.provedTheorems)


contextFormulaCategories : LogicContext -> Formula -> List FormulaCategory
contextFormulaCategories ctx f =
    let
        isAxiom =
            Set.member (toString f) ctx.axiomsLookup

        isNewTheorem =
            ctx.newTheorem == toString f

        isProvedTheorem =
            Set.member (toString f) ctx.provedTheoremsLookup

        results = [(isAxiom, Axiom), (isNewTheorem, NewTheorem), (isProvedTheorem, ProvedTheorem)]
    in
    map (\(_, r) -> r) <| filter (\(b,_) -> b) results
