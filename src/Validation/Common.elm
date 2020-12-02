module Validation.Common exposing(..)

import Dict
import Errors
import Formula exposing (Formula(..))
import Formula.Signed exposing (Signed(..))
import Formula.Parser
import Term exposing (Term(..))
import Helpers.Parser
import Parser
import Set
import Tableau exposing (..)
import Zipper


type ProblemType
    = Syntax
    | Semantics


type alias Problem =
    { typ : ProblemType
    , msg : String
    , zip : Zipper.Zipper
    }


syntaxProblem : a -> b -> List { msg : b, typ : ProblemType, zip : a }
syntaxProblem z s =
    [ { typ = Syntax, msg = s, zip = z } ]


semanticsProblem : a -> b -> List { msg : b, typ : ProblemType, zip : a }
semanticsProblem z s =
    [ { typ = Semantics, msg = s, zip = z } ]


checkFormula :
    String
    -> Zipper.Zipper
    -> Result (List Problem) (Signed Formula)
checkFormula str z =
    z
        |> Zipper.zNode
        |> .formula
        |> Result.mapError (\_ -> semanticsProblem z (str ++ " is invalid."))


checkReffedFormula : String -> Tableau.Ref -> Zipper.Zipper -> Result (List Problem) (Signed Formula)
checkReffedFormula str r z =
    z
        |> Zipper.getReffed r
        |> Result.fromMaybe (semanticsProblem z (str ++ " reference is invalid."))
        |> Result.andThen (checkFormula (str ++ " referenced formula"))


getReffedSignedFormula : (Zipper.Zipper -> Ref) -> Zipper.Zipper -> Result (List Problem) (Signed Formula)
getReffedSignedFormula extractRef z =
    case Zipper.getReffed (extractRef z) z of
        Just rz ->
            case rz |> Zipper.zNode |> .formula of
                Ok sf ->
                    Ok sf

                Err _ ->
                    Err (syntaxProblem z "reffed formula incorrectly parsed")

        Nothing ->
            Err (semanticsProblem z "no reffed formula")


makeSemantic : List { b | typ : ProblemType } -> List { b | typ : ProblemType }
makeSemantic =
    List.map (\p -> { p | typ = Semantics })


resultFromBool : a -> x -> Bool -> Result x a
resultFromBool a x b =
    if b then
        Ok a

    else
        Err x



{- Check the value using a predicate.
   Pass the value along if predicate returns true,
   return the given error otherwise
-}


checkPredicate : (a -> Bool) -> x -> a -> Result x a
checkPredicate pred x a =
    if pred a then
        Ok a

    else
        Err x


isPointingOnSelf : (Zipper.Zipper -> Ref) -> Zipper.Zipper -> Bool
isPointingOnSelf extractRef this =
    case this |> extractRef |> .up of
        Just 0 ->
            True

        _ ->
            False


checkIsPointingOnSelf : (Zipper.Zipper -> Bool) -> x -> Zipper.Zipper -> Result x Zipper.Zipper
checkIsPointingOnSelf pred x z =
    case pred z of
        True ->
            Err x

        False ->
            Ok z