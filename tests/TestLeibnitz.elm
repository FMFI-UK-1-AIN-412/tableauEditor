module TestLeibnitz exposing (..)

import Expect exposing (Expectation)
import Formula exposing (Formula(..))
import Formula.Parser
import Formula.Signed exposing (Signed(..))
import Term exposing (Term(..))
import Tableau exposing (..)
import Test exposing (..)
import Validate
import Validation.Common
import Validation.Rules.Leibnitz
import Zipper exposing (..)


tableauExample =
    { node =
        { id = 1
        , value = ""
        , references = [{ str = "1", up = Just 0 }]
        , formula = Formula.Parser.parseSigned ""
        , gui = defGUI
        }
    , ext = Open
    }


zipperExample =
    ( tableauExample, [] )


suiteLeibnitz : Test
suiteLeibnitz =
    describe "Replacing term with variable"
        [ test "replace variable with variable in eq atom" 
            (\() ->
                Expect.equal
                    (Validation.Rules.Leibnitz.replaceTermWithVar (Var "a") (Var "[]")
                        (T (EqAtom (Var "b") (Var "a")))
                        (T (EqAtom (Var "x") (Var "c")))
                        zipperExample
                    ) 
                    (Ok (T (EqAtom (Var "b") (Var "[]")))) 
            )
        , test "replace function with variable in predicate atom" 
            (\() ->
                Expect.equal
                    (Validation.Rules.Leibnitz.replaceTermWithVar (Fun "f"[Var "b"]) (Var "[]")
                        (T (PredAtom "p" [Fun "f"[Var "b"]] ))
                        (T (PredAtom "p" [Fun "f"[Var "b", Var "x"]] ))
                        zipperExample
                    ) 
                    (Ok (T (PredAtom "p" [Var "[]"] )))
            )
        , test "replace function with variable in predicate atom 2"
            (\() ->
                Expect.equal
                    (Validation.Rules.Leibnitz.replaceTermWithVar (Fun "f"[Var "b"]) (Var "[]")
                        (T (PredAtom "p" [Fun "f"[Fun "f"[Var "b"], Var "b"]] ))
                        (T (PredAtom "p" [Fun "f"[Var "b", Var "b"]] ))
                        zipperExample
                    ) 
                    (Ok (T (PredAtom "p" [Fun "f"[Var "[]", Var "b"]] )))
            )
        , test "no change" 
            (\() ->
                Expect.equal
                    (Validation.Rules.Leibnitz.replaceTermWithVar (Fun "f"[Var "b"]) (Var "[]")
                        (T (Conj (EqAtom (Fun "f"[Var "b"]) (Var "x")) (EqAtom (Fun "f"[Var "b"]) (Var "b")) ) )
                        (T (Conj (EqAtom (Fun "f"[Var "b"]) (Var "x")) (EqAtom (Fun "f"[Var "b"]) (Var "b")) ) )
                        zipperExample
                    ) 
                    (Ok (T (Conj (EqAtom (Fun "f"[Var "b"]) (Var "x")) (EqAtom (Fun "f"[Var "b"]) (Var "b")) ) )) 
            )
        , test "replace in conjunction" 
            (\() ->
                Expect.equal
                    (Validation.Rules.Leibnitz.replaceTermWithVar (Var "c") (Var "[]")
                        (T (Conj (EqAtom (Fun "f"[Var "b", Var "c"]) (Var "x")) (EqAtom (Fun "f"[Var "c"]) (Var "b")) ) )
                        (T (Conj (EqAtom (Fun "f"[Var "b", Var "x"]) (Var "x")) (EqAtom (Fun "f"[Var "x"]) (Var "x")) ) )
                        zipperExample
                    ) 
                    (Ok (T (Conj (EqAtom (Fun "f"[Var "b", Var "[]"]) (Var "x")) (EqAtom (Fun "f"[Var "[]"]) (Var "b")) ) )) 
            )
        , test "replace in for all" 
            (\() ->
                Expect.equal
                    (Validation.Rules.Leibnitz.replaceTermWithVar (Var "c") (Var "[]")
                        (T (ForAll "x" (Disj (EqAtom (Fun "f"[Var "x", Var "z"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "a", Var "b", Var "c"], Fun "g"[Var "a"] ]))))
                       (T (ForAll "x" (Disj (EqAtom (Fun "f"[Var "x", Var "a"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "o", Var "o", Var "o"], Fun "h"[Var "a"] ]))))
                        zipperExample
                    ) 
                    (Ok (T (ForAll "x" (Disj (EqAtom (Fun "f"[Var "x", Var "z"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "a", Var "b", Var "[]"], Fun "g"[Var "a"] ]))))) 
            )
        , test "replace in exists" 
            (\() ->
                Expect.equal
                    (Validation.Rules.Leibnitz.replaceTermWithVar (Fun "f"[Var "c", Var "z"]) (Var "[]")
                        (T (Exists "x" (Conj (EqAtom (Fun "f"[Var "c", Var "z"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "c", Var "z"], Fun "g"[Var "a"] ]))))
                       (T (Exists "x" (Conj (EqAtom (Fun "f"[Var "x", Var "z"]) (Var "y")) (PredAtom "p" [Fun "f"[Var "c", Var "z"], Fun "h"[Var "a"] ]))))
                        zipperExample
                    ) 
                    (Ok (T (Exists "x" (Conj (EqAtom (Var "[]") (Var "y")) (PredAtom "p" [Fun "f"[Var "c", Var "z"], Fun "g"[Var "a"] ]))))) 
            )
        ]