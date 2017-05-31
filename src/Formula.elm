module Formula exposing (..)

import Char
import Set exposing (Set)
import Dict exposing (Dict)
import Parser exposing
  (
    Parser, (|.), (|=), succeed, symbol,
    float, ignore, repeat, zeroOrMore, oneOf, lazy,
    keyword, delayedCommit, delayedCommitMap, end, oneOrMore,
    inContext
  )
import Parser.LanguageKit exposing (variable, sequence, whitespace)
import Result as R
import Errors

type Term
  = Var String
  | Fun String (List Term)

type Formula
  = Atom String (List Term)
  | Neg Formula
  | Disj Formula Formula
  | Conj Formula Formula
  | Impl Formula Formula
  | ForAll String Formula
  | Exists String Formula
  | FF
  | FT

type alias Substitution
  = Dict String Term

subformulas : Formula -> (List Formula)
subformulas f =
  case f of
    Neg sf -> [sf]
    Disj lf rf -> [lf, rf]
    Conj lf rf -> [lf, rf]
    Impl lf rf -> [lf, rf]
    ForAll _ sf -> [sf]
    Exists _ sf -> [sf]
    _ -> []

isSubformulaOf : Formula -> Formula -> Bool
isSubformulaOf a b =
  List.member a (subformulas b)

--
-- First-order syntactic operations
--

freeTermA : Term -> Set String -> Set String
freeTermA t fvs =
  case t of
    Var x -> Set.insert x fvs
    Fun _ ts -> List.foldl freeTermA fvs ts

freeTerm : Term -> Set String
freeTerm t = freeTermA t Set.empty

freeFormula : Formula -> Set String
freeFormula f =
  let
    freeFormulaA : Formula -> Set String -> Set String
    freeFormulaA f fvs =
      case f of
        Atom _ ts -> List.foldl freeTermA fvs ts
        ForAll x sf -> Set.remove x <| freeFormulaA sf fvs
        Exists x sf -> Set.remove x <| freeFormulaA sf fvs
        _ -> List.foldl freeFormulaA fvs <| subformulas f
  in
    freeFormulaA f Set.empty

substTerm : Substitution -> Term -> Term
substTerm sigma t =
  case t of
    Var x -> case Dict.get x sigma of
      Just xt -> xt
      Nothing -> t
    Fun f ts -> Fun f <| List.map (substTerm sigma) ts

unsafeSubstFormula : Substitution -> Formula -> Formula
unsafeSubstFormula sigma f =
  let
    subst = unsafeSubstFormula sigma
  in
    case f of
      Atom p ts -> Atom p (List.map (substTerm sigma) ts)
      ForAll x sf -> ForAll x (unsafeSubstFormula (Dict.remove x sigma) sf)
      Exists x sf -> Exists x (unsafeSubstFormula (Dict.remove x sigma) sf)
      Disj lf rf -> Disj (subst lf) (subst rf)
      Conj lf rf -> Conj (subst lf) (subst rf)
      Impl lf rf -> Impl (subst lf) (subst rf)
      Neg sf -> Neg (subst sf)
      _ -> f

substFormula : Substitution -> Formula -> Result String Formula
substFormula σ f =
  let
    canSubst x t bound =
      let
        clashing = Set.intersect bound (freeTerm t)
        strVars xs = String.join ", " xs
        varsToBe xs =
          "variable" ++ (if Set.size xs == 1 then "" else "s") ++
          " " ++ strVars (Set.toList xs) ++
          (if Set.size xs == 1 then " is" else " are")
      in
        if Set.isEmpty clashing then
          Ok t
        else
          Err <| String.join " "
            [ "Cannot substitute", strTerm t, "for", x ++ ";"
            , varsToBe clashing, "bound"
            ]
    substT : Substitution -> Set String -> Term -> Result String Term
    substT σ bound t =
      let
        subst t =
          case t of
            Var x -> case Dict.get x σ of
              Just xt -> canSubst x xt bound
              Nothing -> Ok t
            Fun f ts ->
              R.map (Fun f) <| substTs σ bound ts
      in
        subst t
    substTs σ bound = Errors.mapResult (substT σ bound)
    substF : Substitution -> Set String -> Formula -> Result String Formula
    substF σ bound f =
      let
        subst = substF σ bound
      in
        case f of
          Atom p ts ->
            R.map (Atom p) (substTs σ bound ts)
          ForAll x sf ->
            R.map (ForAll x)
                  (substF (Dict.remove x σ) (Set.insert x bound) sf)
          Exists x sf ->
            R.map (Exists x)
                  (substF (Dict.remove x σ) (Set.insert x bound) sf)
          Disj lf rf ->
            R.map2 Disj (subst lf) (subst rf)
          Conj lf rf ->
            R.map2 Conj (subst lf) (subst rf)
          Impl lf rf ->
            R.map2 Impl (subst lf) (subst rf)
          Neg sf ->
            R.map Neg (subst sf)
          _ ->
            Ok f
  in
    substF σ Set.empty f

predicates : Formula -> Set String
predicates f =
  let
    predicatesA f ps =
      case f of
        Atom p _ -> Set.insert p ps
        _ -> List.foldl predicatesA ps <| subformulas f
  in
    predicatesA f Set.empty

functions : Formula -> Set String
functions f =
  let
    functionsTA t fs =
      case t of
        Fun f ts -> Set.insert f <| List.foldl functionsTA fs ts
        _ -> fs
    functionsA f fs =
      case f of
        Atom p ts -> List.foldl functionsTA fs ts
        _ -> List.foldl functionsA fs <| subformulas f
  in
    functionsA f Set.empty

variables : Formula -> Set String
variables f =
  let
    variablesTA t vs =
      case t of
        Fun _ ts -> List.foldl variablesTA vs ts
        Var x -> Set.insert x vs
    variablesA f vs =
      case f of
        Atom p ts -> List.foldl variablesTA vs ts
        _ -> List.foldl variablesA vs <| subformulas f
  in
    variablesA f Set.empty

--
-- Signed formulas
--
type Signed a
  = T a
  | F a

type SignedType
  = Alpha
  | Beta
  | Gamma
  | Delta

negType : SignedType -> SignedType
negType t =
  case t of
    Alpha -> Beta
    Beta -> Alpha
    Gamma -> Delta
    Delta -> Gamma

negSigned : (Signed Formula) -> (Signed Formula)
negSigned sf =
  case sf of
    T f -> F f
    F f -> T f

signedType : (Signed Formula) -> SignedType
signedType sf =
  case sf of
    T FF -> Alpha
    T FT -> Alpha
    T (Atom _ _) -> Alpha
    F (Atom _ _) -> Alpha
    T (Neg _) -> Alpha
    F (Neg _) -> Alpha
    T (Conj _ _) -> Alpha
    T (Disj _ _) -> Beta
    T (Impl _ _) -> Beta
    T (ForAll _ _) -> Gamma
    T (Exists _ _) -> Delta
    F f -> negType <| signedType <| T f

isAlpha : (Signed Formula) -> Bool
isAlpha x = Alpha == signedType x

isBeta : (Signed Formula) -> Bool
isBeta x = Beta == signedType x

isGamma : (Signed Formula) -> Bool
isGamma x = Gamma == signedType x

isDelta : (Signed Formula) -> Bool
isDelta x = Delta == signedType x

signedSubformulas : (Signed Formula) -> (List (Signed Formula))
signedSubformulas sf =
  case sf of
    T (Neg f) -> [F f]
    T (Conj l r) -> [T l, T r]
    T (Disj l r) -> [T l, T r]
    T (Impl l r) -> [F l, T r]
    T (ForAll _ f) -> [T f]
    T (Exists _ f) -> [T f]
    T _ -> []
    F f -> T f |> signedSubformulas |> List.map negSigned

isSignedSubformulaOf : (Signed Formula) -> (Signed Formula) -> Bool
isSignedSubformulaOf a b =
  List.member a (signedSubformulas b)

isSignedComplementary : (Signed Formula) -> (Signed Formula) -> Bool
isSignedComplementary a b =
  case (a,b) of
    (T x, F y) -> x == y
    (F x, T y) -> x == y
    _ -> False

signedGetFormula : (Signed Formula) -> Formula
signedGetFormula sf =
  case sf of
    T f -> f
    F f -> f

--
-- Parsing
--
parseSigned = Parser.run (succeed identity |. spaces |= signedFormula |. spaces |. end)

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
parse = Parser.run (succeed identity |. spaces |= formula |. spaces |. end)

errorString : Parser.Error  -> String
errorString e = "Invalid formula: " ++ (toString e)


formula : Parser Formula
formula =
  oneOf
    [ succeed Atom
        |= identifier
        |. spaces
        |= oneOf
            [ inContext "predicate arguments" args
            , succeed []
            ]
    , succeed Neg
        |. oneOfSymbols ["-", "¬", "~"]
        |. spaces
        |= lazy (\_ -> formula)
    , lazy (\_ -> binary ["&", "∧", "/\\"] Conj)
    , lazy (\_ -> binary ["|", "∨", "\\/"] Disj)
    , lazy (\_ -> binary ["->", "→"] Impl)
    , succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> formula)
        |. spaces
        |. symbol ")"
    ]

binary conn constructor =
  delayedCommitMap constructor
  (  succeed identity
     |. symbol "("
     |. spaces
     |= lazy (\_ -> formula)
     |. spaces
  ) <| succeed identity
  |. oneOfSymbols conn
  |. spaces
  |= lazy (\_ -> formula)
  |. spaces
  |. symbol ")"

args : Parser (List Term)
args =
  succeed (::)
    |. symbol "("
    |. spaces
    |= lazy (\_ -> term)
    |. spaces
    |= lazy (\_ -> repeat zeroOrMore nextArg)
    |. symbol ")"

nextArg : Parser Term
nextArg =
  succeed identity
    |. symbol ","
    |. spaces
    |= term
    |. spaces

term : Parser Term
term =
  identifier |>
    Parser.andThen (\name ->
      oneOf
        [ succeed (\args -> Fun name args)
            |= lazy (\_ -> inContext "function arguments" args)
        , succeed (Var name)
        ]
    )

identifier = variable isLetter isIdentChar Set.empty

oneOfSymbols syms =
  oneOf (List.map symbol syms)

isLetter : Char -> Bool
isLetter char =
  Char.isLower char
  || Char.isUpper char

isIdentChar : Char -> Bool
isIdentChar char =
  isLetter char
  || Char.isDigit char
  || char == '_'

spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')


strSigned sf =
  case sf of
    T f -> "T " ++ (strFormula f)
    F f -> "F " ++ (strFormula f)

strFormula f =
  let
    strBinF lf c rf = "(" ++ strFormula lf ++ c ++ strFormula rf ++ ")"
    strQF q bv f = q ++ bv ++ atomSpace f ++ strFormula f
    atomSpace f = case f of
        Atom _ _ -> " "
        _ -> ""
  in
    case f of
      FT -> "True"
      FF -> "False"
      Atom p [] -> p
      Atom p ts -> p ++ strArgs ts
      Neg f -> "¬" ++ strFormula f
      Conj lf rf -> strBinF lf "∧" rf
      Disj lf rf -> strBinF lf "∨" rf
      Impl lf rf -> strBinF lf "→" rf
      ForAll bv f -> strQF "∀" bv f
      Exists bv f -> strQF "∃" bv f

strArgs ts = "(" ++ String.join "," (List.map strTerm ts) ++ ")"

strTerm t =
  case t of
    Var v -> v
    Fun f ts -> f ++ strArgs ts

--
-- helper funcs
--

sf : String -> (Signed Formula)
sf = parseSigned >> Result.withDefault (T FF)
f : String -> Formula
f = parse >> Result.withDefault FF

{- vim: set sw=2 ts=2 sts=2 et : -}
