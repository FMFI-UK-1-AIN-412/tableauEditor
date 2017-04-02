module Validate exposing (..)
import Tableau exposing (..)
import Formula
import Parser
import Errors

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

error : x -> Result x a -> x
error def r =
  case r of
    Err x -> x
    Ok _ -> def

(<++) : (List Problem, Zipper) -> (Zipper -> List Problem) -> (List Problem, Zipper)
(<++) (lp, z) f = (lp ++ f z, z)

(<++?) : (List Problem, Zipper) -> (Zipper -> Result (List Problem) a) -> (List Problem, Zipper)
(<++?) (lp, z) f =
  (lp ++ error [] (f z), z)

second = curry Tuple.second
always3 r _ _ _ = r
always2 r _ _ = r

isCorectTableau : Zipper -> Result (List Problem) Zipper
isCorectTableau z =
  Errors.merge2 (always2 z)
    (isCorrectNode z)
    ( List.foldl
      (Errors.merge2 (always2 z))
      (Ok z)
      (List.map isCorectTableau (children z))
    )

isValidNode : Zipper -> Result (List Problem) Zipper
isValidNode z =
  Errors.merge3 (always3 z)
    (isValidFormula z)
    (isValidNodeRef z)
    (areValidCloseRefs z)

isCorrectNode : Zipper -> Result (List Problem) Zipper
isCorrectNode z =
  isValidNode z
  |> Result.andThen
      (\_ -> Errors.merge2 (second)
        (isCorrectRule z)
        (areCorrectCloseRefs z)
      )

{-| Just for the formula dislplay -- don't check the ref for syntax -}
isCorrectFormula : Zipper -> Result (List Problem) Zipper
isCorrectFormula z =
  isValidFormula z
  |> Result.andThen isCorrectRule

isValidFormula : Zipper -> Result (List Problem) Zipper
isValidFormula z =
  z |> zFormula |> Result.mapError (parseProblem z) |> Result.map (always z)

isValidNodeRef z =
  isValidRef "The" (zNode z).ref z

areValidCloseRefs z =
  case zTableau z of
    Leaf _ (Just (r1, r2)) ->
      Errors.merge2 (always2 z)
        (isValidRef "First close"  r1 z)
        (isValidRef "Second close" r2 z)
    _ -> Ok z

isValidRef str r z =
  r.up
  |> Result.fromMaybe (syntaxProblem z (str ++ " reference is invalid."))
  |> Result.map (always z)

areCorrectCloseRefs z =
  case zTableau z of
    Leaf _ (Just (r1, r2)) ->
      areCloseRefsComplementary r1 r2 z |> Result.map (always z)
    _ -> Ok z


parseProblem : Zipper -> Parser.Error -> List Problem
parseProblem z = Formula.errorString >> syntaxProblem z

validateFormula : Zipper -> Result (List Problem) (Formula.Signed Formula.Formula)
validateFormula z =
  z |> zFormula |> Result.mapError (parseProblem z)

validateReffedFormula : Zipper -> Result (List Problem) (Formula.Signed Formula.Formula)
validateReffedFormula z =
  z |> zFormula |> Result.mapError (\e -> semanticsProblem z "Referenced formula is invalid")

validateRef str r z =
  case r.up of
    Nothing -> syntaxProblem z str
    _ -> []

validateNodeRef z = validateRef "Invalid reference" (zNode z).ref z

checkFormula str z =
  z
  |> zFormula
  |> Result.mapError (\_ -> semanticsProblem z (str ++ " is invalid."))

checkReffedFormula : String -> Tableau.Ref -> Zipper -> Result (List Problem) (Formula.Signed Formula.Formula)
checkReffedFormula str r z =
  z
  |> getReffed r
  |> Result.fromMaybe (semanticsProblem z (str ++ " reference is invalid."))
  |> Result.andThen (checkFormula (str ++ " referenced formula"))

areCloseRefsComplementary r1 r2 z =
  Errors.merge2 Formula.isSignedComplementary
    (checkReffedFormula "First close" r1 z)
    (checkReffedFormula "Second close" r2 z)
  |> Result.andThen (resultFromBool z (semanticsProblem z "Closing formulas are not complementary."))

isCorrectRule ((t, bs) as z) =
  case t |> node |> .ref |> .up of
    Just 0 -> Ok z -- This is a premise
    _ ->
      case bs of
        (AlphaCrumb _)::_ -> validateAlphaRule z
        (BetaLeftCrumb _ _)::_ -> validateBetaRuleLeft z
        (BetaRightCrumb _ _)::_ -> validateBetaRuleRight z
        [] -> Ok z -- Top of the tableau, this must be a premise

makeSemantic = List.map (\p -> { p | typ = Semantics })

resultFromBool : a -> x -> Bool -> Result x a
resultFromBool a x b =
  if b
    then Ok a
    else Err x

validateAlphaRule : Zipper -> Result (List Problem) Zipper
validateAlphaRule z =
  getReffed (zNode z).ref z
  |> Result.fromMaybe (semanticsProblem z "Invalid reference.")
  |> Result.andThen validateReffedFormula
  |> Result.map2 Formula.isSignedSubformulaOf (checkFormula "Formula" z)
  |> Result.andThen (resultFromBool z (semanticsProblem z
      ( "Is not an α-subformula of ("
      ++ toString (getReffed (zNode z).ref z |> Maybe.map (zNode >> .num) |> Maybe.withDefault 0)
      ++ ")."
      )
    ))

validateBetaRuleLeft  z = validateBeta z (z |> up |> right)
validateBetaRuleRight z = validateBeta z (z |> up |> left)

validateBeta this other =
  let
    ft = this  |> checkFormula "Formula"
    fo = other |> checkFormula "The other β subformula"
    children =
      ft
      |> Result.map List.singleton
      |> Result.map2 (::) fo
      |> Result.map (List.sortBy Formula.strSigned) -- This is a hack, but defining an ordering on formulas...
    reffed =
      this
      |> getReffed (zNode this).ref
      |> Result.fromMaybe (semanticsProblem this "Invalid reference")
      |> Result.andThen validateReffedFormula
      |> Result.map Formula.signedSubformulas
      |> Result.map (List.sortBy Formula.strSigned)
  in
    Errors.merge2 (==) children reffed
    |> Result.andThen (resultFromBool this  (semanticsProblem this "Wrong β subformulas."))
    |> Errors.merge2 (always2 this) (betasHaveSameRef this other)


betasHaveSameRef this other =
  let
    -- The invalid refs will be reported already
    getRef = zNode >> .ref >> .up >> Result.fromMaybe []
    rt = getRef this
    ro = getRef other
  in
    Errors.merge2 (==) rt ro
    |> Result.andThen
        (resultFromBool this (semanticsProblem this "β references are not the same"))

{- vim: set sw=2 ts=2 sts=2 et :s  -}
