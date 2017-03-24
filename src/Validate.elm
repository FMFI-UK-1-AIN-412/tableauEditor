module Validate exposing (..)
import Tableau exposing (..)
import Formula
import Parser
import Set

type ProblemType
  = Syntax
  | Semantics

type alias Problem =
  { typ : ProblemType
  , msg : String
  , zip : Zipper
  }

syntaxProblem z s = [ { typ = Syntax, msg = s, zip = z} ]
semanticsProblem z s = [ { typ = Semantics, msg = s, zip = z} ]

(<++) : (List Problem, Zipper) -> (Zipper -> List Problem) -> (List Problem, Zipper)
(<++) (lp, z) f = (lp ++ f z, z)

validate : Zipper -> List Problem
validate z =
  Tuple.first <| ([], z)
  <++ validateNode
  <++ (\z -> List.concat (List.map validate (children z)))

validateNode : Zipper -> List Problem
validateNode z =
  Tuple.first <| ([], z)
  <++ validateFormula
  <++ validateNodeRef
  <++ validateClosedRefs
  <++ validateRule

validateFormula : Zipper -> List Problem
validateFormula z =
  case zFormula z of
    Err e -> syntaxProblem z (Formula.errorString e)
    _ -> []

validateNodeRef z = validateRef "Invalid reference" z (zNode z).ref

validateClosedRefs ((t, bs) as z) =
  case t of
    Leaf _ (Just (r1, r2)) ->
      []
      ++ validateRef "Invalid close ref. #1" z r1
      ++ validateRef "Invalid close ref. #2" z r2
      ++ validateClosedCompl z r1 r2
    _ -> []


validateClosedCompl z r1 r2 =
  Maybe.map2 Formula.isSignedComplementary (getReffedFormula z r1) (getReffedFormula z r2)
  |> Maybe.map (\b -> if b then [] else (semanticsProblem z "Closing formulas are not complementary"))
  |> Maybe.withDefault (semanticsProblem z "Referenced formulas are invalid")

validateRef str z r =
  case r.up of
    Nothing -> syntaxProblem z str
    _ -> []

validateRule ((t, bs) as z) =
  case bs of
    (AlphaCrumb _)::_ -> validateAlphaRule z
    (BetaLeftCrumb _ _)::_ -> validateBetaRuleLeft z
    (BetaRightCrumb _ _)::_ -> validateBetaRuleRight z
    [] -> []

ifFalse : a -> Bool -> Maybe a
ifFalse a b =
  if not b
    then Just a
    else Nothing

validateAlphaRule : Zipper ->List Problem
validateAlphaRule z =
  getReffed z (zNode z).ref
  |> Maybe.andThen (zFormula >> Result.toMaybe)
  |> Maybe.map2 Formula.isSignedSubformulaOf (zFormula z |> Result.toMaybe)
  |> Maybe.andThen (ifFalse <| semanticsProblem z
      ( "Is not an α-subformula of ("
      ++ toString ((zNode z).ref |> getReffed z |> Maybe.map (zNode >> .num) |> Maybe.withDefault 0)
      ++ ")"
      )
    )
  |> Maybe.withDefault []

validateBetaRuleLeft  z = validateBeta z (z |> up |> right)
validateBetaRuleRight z = validateBeta z (z |> up |> left)

validateBeta this other =
  let
    mt = this  |> zFormula |> Result.toMaybe
    mo = other |> zFormula |> Result.toMaybe
    children =
      mt
      |> Maybe.map List.singleton
      |> Maybe.map2 (::) mo
      |> Maybe.map (List.sortBy Formula.strSigned) -- This is a hack, but defining an ordering on formulas...
    reffed =
      getReffed this (zNode this).ref
      |> Maybe.andThen (zFormula >> Result.toMaybe)
      |> Maybe.map Formula.signedSubformulas
      |> Maybe.map (List.sortBy Formula.strSigned)
  in
    Maybe.map2 (==) children reffed
    |> Maybe.andThen (ifFalse (semanticsProblem this "Wrong beta"))
    |> Maybe.withDefault []



