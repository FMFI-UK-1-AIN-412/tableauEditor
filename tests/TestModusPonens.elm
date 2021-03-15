module TestModusPonens exposing (..)

import Expect exposing (Expectation)
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Test exposing (..)
import Validation.Common exposing (..)
import Validation.Rules.ModusPonens exposing (check)
import Zipper exposing (..)


zipperWithFormula f = 
    let 
        tableau = 
            { node =
                { id = 1
                , value = ""
                , references = [{ str = "1", up = Just 0 }]
                , formula = Formula.Parser.parseSigned f
                , gui = defGUI
                }
            , ext = Open
            }
    in
    zipper tableau

z1 = zipperWithFormula "T b"
z2 = zipperWithFormula "T ∀c r(c)"

z3 = zipperWithFormula "T (p(x) -> r(x))"

suiteCheck : Test
suiteCheck =
    describe "the check function in MP"
        [ test "simple impl" 
            (\() ->
                Expect.equal
                    (check
                        (T (Impl (PredAtom "a" [])(PredAtom "b" [])))
                        (T (PredAtom "a" []))
                        z1
                    ) 
                    (Ok z1)
            )
        , test "impl with quant" 
            (\() ->
                Expect.equal
                    (check
                        (T (Exists "x" (PredAtom "p" [Var "x"])))
                        (T (Impl (Exists "x" (PredAtom "p" [Var "x"]))(ForAll "c" (PredAtom "r" [Var "c"]))))
                        z2
                    ) 
                    (Ok z2) 
            )
        , test "impl of impls" 
            (\() ->
                Expect.equal
                    (check
                        (T (Impl (Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"])))(Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"])))))
                        (T (Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"]))))
                        z3
                    ) 
                    (Ok z3) 
            )
        , test "wrong ref formulas1" 
            (\() ->
                Expect.equal
                    (check
                        (T (Impl (PredAtom "a" [])(PredAtom "b" [])))
                        (F (PredAtom "a" []))
                        z1
                    ) 
                    (Err (semanticsProblem z1 "MP rule can't be used on referenced formulas")) 
            )
        , test "wrong ref formulas2" 
            (\() ->
                Expect.equal
                    (check
                        (T (Exists "x" (PredAtom "p" [Var "x"])))
                        (T (Impl (Exists "x" (PredAtom "p" [Var "z"]))(ForAll "c" (PredAtom "r" [Var "c"]))))
                        z2
                    ) 
                    (Err (semanticsProblem z2 "MP rule can't be used on referenced formulas")) 
            )
        , test "wrong ref formulas3" 
            (\() ->
                Expect.equal
                    (check
                        (T (Impl (Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"])))(Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"])))))
                        (T (Conj ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"]))))
                        z3
                    ) 
                    (Err (semanticsProblem z3 "MP rule can't be used on referenced formulas")) 
            )
        , test "wrong new formula1" 
            (\() ->
                Expect.equal
                    (check
                        (T (Impl (PredAtom "b" [])(PredAtom "a" [])))
                        (T (PredAtom "b" []))
                        z1
                    ) 
                    (Err (semanticsProblem z1 "Formula was not created using the MP rule")) 
            )
        , test "wrong new formula2" 
            (\() ->
                Expect.equal
                    (check
                        (T (ForAll "c" (PredAtom "r" [Var "c"])))
                        (T (Impl (ForAll "c" (PredAtom "r" [Var "c"])) (Exists "x" (PredAtom "p" [Var "x"]))))
                        z2
                    ) 
                    (Err (semanticsProblem z2 "Formula was not created using the MP rule")) 
            )
        , test "wrong new formula3" 
            (\() ->
                Expect.equal
                    (check
                        (T (Impl (Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"])))(Impl ((EqAtom (Var "x") (Var "p")))((PredAtom "r" [Var "x"])))))
                        (T (Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"]))))
                        z3
                    ) 
                    (Err (semanticsProblem z3 "Formula was not created using the MP rule")) 
            )
        ]


