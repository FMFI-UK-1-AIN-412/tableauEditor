module Formula exposing (..)

import Char
import Set
import Parser exposing
  (
    Parser, (|.), (|=), succeed, symbol,
    float, ignore, zeroOrMore, oneOf, lazy,
    keyword, delayedCommitMap, end, oneOrMore
  )
import Parser.LanguageKit exposing (variable, sequence, whitespace)

type Formula
  = Atom String
  | Neg Formula
  | Disj Formula Formula
  | Conj Formula Formula
  | Impl Formula Formula
  | FF
  | FT

subformulas : Formula -> (List Formula)
subformulas f =
  case f of
    Neg sf -> [sf]
    Disj lf rf -> [lf, rf]
    Conj lf rf -> [lf, rf]
    Impl lf rf -> [lf, rf]
    _ -> []

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
    Alpha -> Beta
    Beta -> Alpha


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
    T (Atom _) -> Alpha
    T (Neg _) -> Alpha
    T (Conj _ _) -> Alpha
    T (Disj _ _) -> Beta
    T (Impl _ _) -> Beta
    F f -> negType <| signedType <| T f

signedSubformulas : (Signed Formula) -> (List (Signed Formula))
signedSubformulas sf =
  case sf of
    T (Neg f) -> [F f]
    T (Conj l r) -> [T l, T r]
    T (Disj l r) -> [T l, T r]
    T (Impl l r) -> [F l, T r]
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
        |= variable Char.isLower Char.isLower (Set.fromList [])
    , succeed Neg
        |. keyword "-"
        |. spaces
        |= lazy (\_ -> formula)
    , lazy (\_ -> binary ["&", "∧"] Conj)
    , lazy (\_ -> binary ["|", "∨"] Disj)
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
  |. oneOf (List.map symbol conn)
  |. spaces
  |= lazy (\_ -> formula)
  |. spaces
  |. symbol ")"


spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')


strSigned sf =
  case sf of
    T f -> "T " ++ (strFormula f)
    F f -> "F " ++ (strFormula f)

strFormula f =
  case f of
    FT -> "True"
    FF -> "False"
    Atom a -> a
    Neg f -> "¬" ++ (strFormula f)
    Conj lf rf -> "(" ++ (strFormula lf) ++ "∧" ++ (strFormula rf) ++ ")"
    Disj lf rf -> "(" ++ (strFormula lf) ++ "∨" ++ (strFormula rf) ++ ")"
    Impl lf rf -> "(" ++ (strFormula lf) ++ "→" ++ (strFormula rf) ++ ")"

--
-- helper funcs
--

sf : String -> (Signed Formula)
sf = parseSigned >> Result.withDefault (T FF)
f : String -> Formula
f = parse >> Result.withDefault FF

{- vim: set sw=2 ts=2 sts=2 et : -}
