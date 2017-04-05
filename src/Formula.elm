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
  | FF
  | FT
  | Neg Formula
  | Disj Formula Formula
  | Conj Formula Formula
  | Impl Formula Formula


type Signed a
  = T a
  | F a


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

formula : Parser Formula
formula =
  oneOf
    [ succeed Atom
        |= variable Char.isLower Char.isLower (Set.fromList [])
    , succeed Neg
        |. keyword "-"
        |. spaces
        |= lazy (\_ -> formula)
    , lazy (\_ -> binary "&" Conj)
    , lazy (\_ -> binary "|" Disj)
    , lazy (\_ -> binary "->" Impl)
    ]

conjunction = lazy (\_ -> binary "&" Conj)
disjunction = lazy (\_ -> binary "|" Conj)

binary conn constructor =
  delayedCommitMap constructor
  (  succeed identity
     |. symbol "("
     |. spaces
     |= lazy (\_ -> formula)
     |. spaces
  ) <| succeed identity
  |. symbol conn
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

