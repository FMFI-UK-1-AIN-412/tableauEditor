module TestDS exposing (..)

import Expect exposing (Expectation)
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Test exposing (..)
import Validation.Common exposing (..)
import Validation.Rules.DS exposing (structureOne, structureTwo)
import Zipper exposing (..)
import FontAwesome exposing (check)


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

z1 = zipperWithFormula "T (a | b)"
z2 = zipperWithFormula "T a = f(b)"
z3 = zipperWithFormula "T ∀x(p(x) & r(x))"

suiteTryBothOrdersAndStructures : Test
suiteTryBothOrdersAndStructures =
    describe "validation with tryBothOrdersAndStructures function in DS"
        [ test "first structure with disj" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (T (Disj (PredAtom "b" [])(Disj((PredAtom "a" []))((PredAtom "b" [])))))
                        (F (PredAtom "b" []))
                        z1
                    ) 
                    (Ok z1)
            )
        , test "second structure withequality" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (F (ForAll "c" (PredAtom "r" [Var "c"])))
                        (T (Disj (EqAtom (Var "a") (Fun "f" [Var "b"]))(ForAll "c" (PredAtom "r" [Var "c"]))))
                        z2
                    ) 
                    (Ok z2) 
            )
        , test "first structure with forall conj" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (T (Disj (Neg(EqAtom (Var "a")(Var "b")))(ForAll "x" (Conj (PredAtom "p" [Var "x"]) (PredAtom "r" [Var "x"])))))
                        (F (Neg(EqAtom (Var "a")(Var "b"))))
                        z3
                    ) 
                    (Ok z3) 
            )
        , test "wrong ref formulas1" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (T (Disj (PredAtom "a" [])(PredAtom "b" [])))
                        (T (PredAtom "a" []))
                        z1
                    ) 
                    (Err (semanticsProblem z1 "DS rule can't be used on referenced formulas")) 
            )
        , test "wrong ref formulas2" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (F (Exists "c" (PredAtom "p" [Var "x"])))
                        (T (Impl (Exists "x" (PredAtom "p" [Var "z"]))(Exists "c" (PredAtom "p" [Var "c"]))))
                        z2
                    ) 
                    (Err (semanticsProblem z2 "DS rule can't be used on referenced formulas")) 
            )
        , test "wrong ref formulas3" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (T (Disj (Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"])))(Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"])))))
                        (F (Conj ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"]))))
                        z3
                    ) 
                    (Err (semanticsProblem z3 "DS rule can't be used on referenced formulas")) 
            )
        , test "wrong new formula1" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (T (Disj (Disj(PredAtom "b" [])(PredAtom "b" []))(PredAtom "a" [])))
                        (F (Disj(PredAtom "b" [])(PredAtom "b" [])))
                        z1
                    ) 
                    (Err (semanticsProblem z1 "formula was not created using the DS rule")) 
            )
        , test "wrong new formula2" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (F (ForAll "c" (PredAtom "r" [Var "c"])))
                        (T (Disj (ForAll "c" (PredAtom "r" [Var "c"])) (Exists "x" (PredAtom "p" [Var "x"]))))
                        z2
                    ) 
                    (Err (semanticsProblem z2 "formula was not created using the DS rule")) 
            )
        , test "wrong new formula3" 
            (\() ->
                Expect.equal
                    (tryBothOrdersAndStructures structureOne structureTwo
                        (T (Disj (Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"]))) (ForAll "x" (Conj (PredAtom "p" [Var "x"]) (PredAtom "p" [Var "x"])))))
                        (F (Impl ((PredAtom "p" [Var "x"]))((PredAtom "r" [Var "x"]))))
                        z3
                    ) 
                    (Err (semanticsProblem z3 "formula was not created using the DS rule")) 
            )
        ]


