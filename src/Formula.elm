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
        |= variable Char.isLower isIdentChar (Set.fromList [])
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
  variable isLetter isIdentChar Set.empty |>
    Parser.andThen (\name ->
      oneOf
        [ succeed (\args -> Fun name args)
            |= lazy (\_ -> inContext "function arguments" args)
        , succeed (Var name)
        ]
    )

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
  case f of
    FT -> "True"
    FF -> "False"
    Atom p ts -> if List.isEmpty ts then
                   p
                 else
                   p ++ strArgs ts
    Neg f -> "¬" ++ (strFormula f)
    Conj lf rf -> "(" ++ (strFormula lf) ++ "∧" ++ (strFormula rf) ++ ")"
    Disj lf rf -> "(" ++ (strFormula lf) ++ "∨" ++ (strFormula rf) ++ ")"
    Impl lf rf -> "(" ++ (strFormula lf) ++ "→" ++ (strFormula rf) ++ ")"
    ForAll bv rf -> "∀" ++ bv ++ (strFormula rf)
    Exists bv rf -> "∃" ++ bv ++ (strFormula rf)

strArgs ts = "(" ++ (String.join "," <| List.map strTerm ts) ++ ")"

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
