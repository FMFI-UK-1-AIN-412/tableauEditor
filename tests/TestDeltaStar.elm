module TestDeltaStar exposing (..)

import Expect 
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Test exposing (..)
import Validation.Common exposing (..)
import Zipper exposing (..)
import Dict


z = 
    let 
        tableau = 
            { node =
                { id = 1
                , value = ""
                , references = [{ str = "1", up = Just 0 }]
                , formula = Formula.Parser.parseSigned ""
                , gui = defGUI
                }
            , ext = Open
            }
    in
    zipper tableau


suiteCheckNewFormula : Test
suiteCheckNewFormula =
    describe "checkNewFormula with delta rule"
        [ test "double subst" 
            (\() ->
                Expect.ok
                    (unaryWithSubstCheck
                        "δ*"
                        (T (Exists "x" (Exists "y" (PredAtom "P" [Var "x", Var "y"]))))
                        (T (PredAtom "P" [Var "a", Var "o"]))
                        (Dict.fromList [("x", Var "a"),("y", Var "o")])
                        z
                    ) 
            )
        , test "just removing quantifiers" 
            (\() ->
                Expect.ok
                    (unaryWithSubstCheck
                        "δ*"
                        (T (Exists "x" (Exists "y" (PredAtom "P" [Var "x", Var "y"]))))
                        (T (PredAtom "P" [Var "x", Var "y"]))
                        (Dict.fromList [("x", Var "x"),("y", Var "y")])
                        z
                    ) 
            )
        , test "single subst" 
            (\() ->
                Expect.ok
                    (unaryWithSubstCheck
                        "δ*"
                        (T (Exists "x" (Exists "y" (PredAtom "P" [Var "x", Var "y"]))))
                        (T (Exists "y" (PredAtom "P" [Var "z", Var "y"])))
                        (Dict.fromList [("x", Var "z")])
                        z
                    ) 
            )
        , test "triple subst" 
            (\() ->
                Expect.ok
                    (unaryWithSubstCheck
                        "δ*"
                        (T (Exists "z"(Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "z"]))))))
                        (T (ForAll "m"(PredAtom "P" [Var "a", Var "b", Var "c"])))
                        (Dict.fromList [("x", Var "a"), ("y", Var "b"),("z", Var "c")])
                        z
                    ) 
            )
        , test "not substitutable" 
            (\() ->
                Expect.equal
                    (unaryWithSubstCheck
                        "δ*"
                        (T (Exists "z"(Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "z"]))))))
                        (T (Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "y"])))))
                        (Dict.fromList [("z", Var "y")])
                        z
                    ) 
                    (Err (semanticsProblem z "Cannot substitute y for z; variable y is bound"))
            )
        , test "not removed quant" 
            (\() ->
                Expect.equal
                    (unaryWithSubstCheck
                        "δ*"
                        (T (Exists "z"(Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "z"]))))))
                        (T (Exists "z"(Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "o"]))))))
                        (Dict.fromList [("z", Var "o")])
                        z
                    ) 
                    (Err (semanticsProblem z "The variable 'z' can't be substituted for"))
            )
        , test "not removed quant2" 
            (\() ->
                Expect.equal
                    (unaryWithSubstCheck
                        "δ*"
                        (T (Exists "z"(Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "z"]))))))
                        (T (Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "o"])))))
                        (Dict.fromList [("z", Var "o"), ("x", Var "x")])
                        z
                    ) 
                    (Err (semanticsProblem z "The variable 'x' can't be substituted for"))
            )
        , test "different sign" 
            (\() ->
                Expect.equal
                    (unaryWithSubstCheck
                        "δ*"
                        (T (Exists "z"(Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "z"]))))))
                        (F (Exists "x" (Exists "y" (ForAll "m"(PredAtom "P" [Var "x", Var "y", Var "o"])))))
                        (Dict.fromList [("z", Var "o")])
                        z
                    ) 
                    (Err (semanticsProblem z "Formula was not created by using the δ* rule"))
            )
        ]



